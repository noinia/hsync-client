module HSync.Client.Types where

import qualified System.FilePath as FP
import HSync.Client.Import.NoFoundation
import Control.Lens
import qualified Data.Text as T


type URL = Text

data DuchyConfig = DuchyConfig { _serverRoot :: URL
                               , _username   :: UserName
                               , _password   :: Password
                               , _realm      :: RealmId
                               , _baseDir    :: Path
                               }
                 deriving (Show,Eq)
makeClassy ''DuchyConfig


data SyncConfig = SyncConfig { _duchy        :: DuchyConfig
                             , _localBaseDir :: FilePath
                             }
            deriving (Show,Eq)
makeClassy ''SyncConfig

instance HasDuchyConfig SyncConfig where
  duchyConfig = duchy


asLocalPath            :: SyncConfig -> Path -> Maybe FilePath
asLocalPath s (Path p) = (FP.joinPath . (s^.localBaseDir :)) <$> strippedRBD
  where
    (Path rbd)   = s^.baseDir
    strippedRBD = map (T.unpack . _unFileName) <$> stripPrefix rbd p


asRemotePath      :: SyncConfig -> FilePath -> Maybe (RealmId,Path)
asRemotePath s fp = (s^.realm,) . Path . map (FileName . T.pack) . FP.splitDirectories
                 <$> stripPrefix (s^.localBaseDir) fp
