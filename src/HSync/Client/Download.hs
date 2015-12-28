module HSync.Client.Download where

import Network
import HSync.Client.Import hiding (newManager)
import HSync.Client.Actions
import Network.HTTP.Conduit(tlsManagerSettings, newManager)
import System.Environment
import System.Directory(getXdgDirectory, XdgDirectory(..))
import System.FilePath((</>))

main = withSocketsDo $ getConfigs >>= \(hc,sc) -> withSync hc sc $ do
    b <- login
    liftIO $ print b
    -- storeDirectory (Path ["foo"])
    tr <- getCurrentRealm (Path [])
    liftIO $ print tr
    downloadCurrent (Path [])

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

withSync           :: HSyncConfig -> SyncConfig -> Action () -> IO ()
withSync hc sc act = do
    mgr <- newManager tlsManagerSettings
    let sync = Sync hc sc mgr
        -- s    = tIgnores
                                                   -- r    = AcidSync acid
    runActionT act sync
