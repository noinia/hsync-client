module HSync.Client.Foundation where

import Control.Monad.State.Class
import Control.Lens
import HSync.Client.Import.NoFoundation hiding (newManager)
import HSync.Client.Types
import HSync.Client.ServerState
import HSync.Client.AcidState
import Data.Acid(query)
import HSync.Client.ActionT
import Network.HTTP.Conduit(Manager, tlsManagerSettings, newManager)
import qualified Yesod.Client as YC
import System.Directory(getXdgDirectory, XdgDirectory(..))
import System.FilePath((</>))


--------------------------------------------------------------------------------

-- | A sync is a data type that represents the part of our application that connects
-- to a server, and keeps a single directory tree in sync with a remote server.
data Sync = Sync { _hsyncConfig :: HSyncConfig
                 , _config      :: SyncConfig
                 , _manager     :: Manager
                 , _clientId    :: Maybe ClientId
                 , _acids       :: Acids
                    -- Add some task priority queue
                 }
makeLenses ''Sync

instance HasSyncConfig Sync where
  syncConfig = config

instance HasDuchyConfig Sync where
  duchyConfig = config.duchy


instance YC.IsYesodClient Sync where
  type API Sync = HSyncAPI
  serverAppRoot = (^.serverRoot)
  manager       = (^.manager)
  api           = const HSyncAPI


instance HasAcidState (ActionT Sync (ResourceT IO)) ServerState where
  getAcidState = _serverState . _acids <$> get

type Action = ActionT Sync (ResourceT IO)



-- | Data type to represent the entire Application
data HSyncClient = HSyncClient { _settings :: Settings
                               -- , _acids    :: AcidState
                               }
makeLenses ''HSyncClient



--------------------------------------------------------------------------------
-- * Starting The application

getAppDir :: IO FilePath
getAppDir = getXdgDirectory XdgConfig "hsync-client"

getConfigs :: IO (HSyncConfig,SyncConfig)
getConfigs = do
    appDir <- getAppDir
    ehsC <- readConfig $ appDir </> "config" </> "hsync.yaml"
    essC <- readConfig $ appDir </> "config" </> "test.yaml"
    case (ehsC, essC) of
      (Right hsC, Right ssC) -> return (hsC,ssC)
      x -> print x >> error "foo"
      -- (Left e, _)  -> putStrLn e >> error "?"
      -- (_, Left e)  -> putStrLn e >> error "/"

withSync                  :: HSyncConfig -> SyncConfig -> Acids -> Action () -> IO ()
withSync hc sc acids' act = do
    mgr <- newManager tlsManagerSettings
    mci <- query (acids'^.serverState) QueryClientId
    let sync = Sync hc sc mgr mci acids'
    runActionT act sync

getStateDir    :: SyncConfig -> IO FilePath
getStateDir sc = (</> "state" </> sc^.duchyName) <$> getAppDir

runMain     :: Action () -> IO ()
runMain act = do
    (hc,sc) <- getConfigs
    sd      <- getStateDir sc
    withAcids (Just sd) $ \acids' -> withSync hc sc acids' act
