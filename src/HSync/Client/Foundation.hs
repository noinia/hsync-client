module HSync.Client.Foundation where

import HSync.Client.Import.NoFoundation
import HSync.Client.Types
import HSync.Client.ActionT
import Network.HTTP.Conduit(Manager)
import Control.Lens
import qualified Yesod.Client as YC


--------------------------------------------------------------------------------

data Settings = Settings { _duchies :: [DuchyConfig]
                         }
                deriving (Show,Eq)
makeLenses ''Settings

data Sync = Sync { _config     :: SyncConfig
                 , _manager    :: Manager
                 , _clientName :: ClientName
                 }
makeLenses ''Sync

instance HasSyncConfig Sync where
  syncConfig = config

instance HasDuchyConfig Sync where
  duchyConfig = config.duchy


type Action = ActionT Sync (ResourceT IO)


data HSyncClient = HSyncClient { _settings :: Settings
                               -- , _acids    :: AcidState
                               }
makeLenses ''HSyncClient


instance YC.IsYesodClient Sync where
  type API Sync = HSyncAPI
  serverAppRoot = (^.serverRoot)
  manager       = (^.manager)
  api           = const HSyncAPI