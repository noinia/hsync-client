module HSync.Client.Foundation where

import HSync.Client.Import.NoFoundation
import HSync.Client.Types
import HSync.Client.ActionT
import Network.HTTP.Conduit(Manager)
import Control.Lens
import qualified Yesod.Client as YC


--------------------------------------------------------------------------------

-- | A sync is a data type that represents the part of our application that connects
-- to a server, and keeps a single directory tree in sync with a remote server.
data Sync = Sync { _hsyncConfig :: HSyncConfig
                 , _config      :: SyncConfig
                 , _manager     :: Manager
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



type Action = ActionT Sync (ResourceT IO)



-- | Data type to represent the entire Application
data HSyncClient = HSyncClient { _settings :: Settings
                               -- , _acids    :: AcidState
                               }
makeLenses ''HSyncClient
