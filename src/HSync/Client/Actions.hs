module HSync.Client.Actions where

import System.Directory
import Control.Lens hiding ((<.>))
import HSync.Client.Import
import HSync.Common.Header
import Data.Aeson
import Data.Aeson.Types(parseEither)
import Data.Aeson.Parser(value')

import qualified Data.Conduit as C




login :: Action Bool
login = do
    sync <- get
    resp <- liftYT $ runRouteWith APILoginR ( addHeader' HUserName (sync^.username)
                                            . addHeader' HPassword (sync^.password)
                                            . setMethod methodPost
                                            )
    body <- bodyOf resp
    case body of
      "true"  -> do setSessionCreds resp
                    pure True
      _       -> pure False -- TODO: Print what went wrong
  where
    addHeader' h v r = r { requestHeaders = asHeader h v : requestHeaders r }
    setMethod    m r = r { method = m }

setSessionCreds :: Response body -> Action ()
setSessionCreds = liftYT . updateCookieJar


bodyOf resp = lift $ responseBody resp C.$$+- sinkLazy



listenNow = error "not implemented yet"


listenFrom = error "not implemented yet"

getCurrentRealm = error "not implemented yet"

getFile     :: Signature -> Path -> Action ()
getFile s p = toLocalPath p >>= getFile' s p

toLocalPath   :: Path -> Action FilePath
toLocalPath p = do
  sync <- get
  case asLocalPath (sync^.config) p of
    Just fp -> pure fp
    Nothing -> error "TODO: Throw some exception or so?"

getFile'        :: Signature -> Path -> FilePath -> Action ()
getFile' s p fp = do
    sync <- get
    resp <- runGetRoute $ FileR (sync^.realm) s p
    -- Download the file into a partial file
    let fpPartial = fp <.> partialFileExtension
    -- debugM "Actions.getFile" $ "Downloading into " <> show lpPartial
    lift $ responseBody resp C.$$+- sinkFile fpPartial
    -- debugM "Actions.getFile" $ "Download complete."
    -- debugM "Actions.getFile" "Moving partial file to actual path."
    liftIO $ renameFile fpPartial fp
    -- liftIO $ print "Should have renamed now "



--     -- set the modification time of the file we just downloaded
--     setModificationTimeBy s fp

-- setModificationTime      :: Signature -> FilePath -> Action ()
-- setModificationTime s fp = let t = toEpochTime . getDateTime $ fi in
--   toLocalPath p >>= (\fp -> liftIO $ setFileTimes (encodeString fp) t t )


downloadCurrent :: Path -> Action ()
downloadCurrent = error "not implemented yet"



downloadVersion = error "not implemented yet"

storeDirectory   :: Path -> Action ()
storeDirectory p = do
    sync <- get
    resp <- runPostRoute (CreateDirR (sync^.hsyncConfig.clientName) (sync^.realm) p)
                         mempty
    body <- bodyOf resp
    case eitherDecode body of
      Right (Right n) -> print (n :: Notification)
      Right (Left e)  -> print (e :: Text)
      Left  e         -> print e

storeFile = error "not implemented yet"

delete = error "not implemented yet"
