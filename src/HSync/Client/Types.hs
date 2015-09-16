module HSync.Client.Types where

import qualified System.FilePath as FP
import System.FilePath.GlobPattern(GlobPattern)
import HSync.Client.Import.NoFoundation
import Control.Lens
import qualified Data.Text as T
import Data.Yaml

--------------------------------------------------------------------------------

-- | An URL
type URL = Text


-- | Files that should be temporarily ignored (in order to avoid reuploading
--   incoming files)
type TemporaryIgnoreFiles = Set FilePath


type IgnoredPatterns = Set GlobPattern

--------------------------------------------------------------------------------
-- * The setting types for a single Sync

-- | The Part of the remote Realm that we are interested in
data DuchyConfig = DuchyConfig { _serverRoot :: URL
                               , _username   :: UserName
                               , _password   :: Password
                               , _realm      :: RealmId
                               , _baseDir    :: Path
                               }
                 deriving (Show,Eq)
makeClassy ''DuchyConfig


parsePath :: Text -> Parser Path
parsePath = pure . Path  . map (FileName . T.pack . FP.dropTrailingPathSeparator)
          . FP.splitPath . T.unpack


instance FromJSON DuchyConfig where
  parseJSON (Object v) = DuchyConfig <$>  v .:  "server"
                                     <*>  v .:  "username"
                                     <*>  v .:  "password"
                                     <*>  v .:  "realm"
                                     <*> (v .:  "remoteBaseDir" >>= parsePath)
  parseJSON _          = mzero





-- | The settings for a single sync
data SyncConfig = SyncConfig { _duchy        :: DuchyConfig
                             , _localBaseDir :: FilePath
                             }
            deriving (Show,Eq)
makeClassy ''SyncConfig

instance HasDuchyConfig SyncConfig where
  duchyConfig = duchy


instance FromJSON SyncConfig where
  parseJSON j@(Object v) = SyncConfig <$> parseJSON j
                                      <*> v .:  "localBaseDir"
  parseJSON _            = mzero

-- | The config/settings of the application that we are passing to a sync
data HSyncConfig = HSyncConfig { _clientName :: ClientName
                               }
                   deriving (Show,Eq)
makeLenses ''HSyncConfig

instance FromJSON HSyncConfig where
  parseJSON (Object v) = HSyncConfig <$> v .:  "clientName"
  parseJSON _          = mzero




----------------------------------------
-- ** Functions on Duchy's


asLocalPath            :: SyncConfig -> Path -> Maybe FilePath
asLocalPath s (Path p) = (FP.joinPath . (s^.localBaseDir :)) <$> strippedRBD
  where
    (Path rbd)   = s^.baseDir
    strippedRBD = map (T.unpack . _unFileName) <$> stripPrefix rbd p


asRemotePath      :: SyncConfig -> FilePath -> Maybe (RealmId,Path)
asRemotePath s fp = (s^.realm,) . Path . map (FileName . T.pack) . FP.splitDirectories
                 <$> stripPrefix (s^.localBaseDir) fp

--------------------------------------------------------------------------------
-- * Global application settings


-- | Settings for the global application.
data Settings = Settings { _duchies :: [DuchyConfig]
                         }
                deriving (Show,Eq)
makeLenses ''Settings

readConfig    :: FromJSON a => FilePath -> IO (Either String a)
readConfig fp = either (Left . prettyPrintParseException) Right <$> decodeFileEither fp
