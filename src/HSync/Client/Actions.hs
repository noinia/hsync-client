module HSync.Client.Actions where

import System.Directory
import Control.Lens hiding ((<.>))
import HSync.Client.Import
import HSync.Common.Header
import HSync.Common.Zip(writeArchive)
import Data.Aeson
import Data.Aeson.Types(parseEither)
import Data.Aeson.Parser(value')
import Data.Conduit.Attoparsec(sinkParser, conduitParser)
import qualified Data.Attoparsec.Types      as AP
import qualified Data.Conduit               as C
import qualified Data.Conduit.List          as CL



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

bodyOf      :: LazySequence b a
            => Response (ResumableSource (ResourceT IO) a) -> Action b
bodyOf resp = lift $ responseBody resp $$+- sinkLazy


listenNow  :: Path -> Action (ResumableSource (ResourceT IO) PublicNotification)
listenNow p = do
    sync <- get
    listenFromWith (ListenNowR (sync^.realm) p) (not . isMyAction sync)

listenFrom      :: DateTime -> Path
                -> Action (ResumableSource (ResourceT IO) PublicNotification)
listenFrom dt p = do
    sync <- get
    listenFromWith (ListenR dt (sync^.realm) p) (not . isMyAction sync)


isMyAction        :: Sync -> PublicNotification -> Bool
isMyAction sync n =   n^.event.newVersion.lastModified.modClient
                   /= Just (sync^.hsyncConfig.clientName)

listenFromWith            :: Route HSyncAPI -> (PublicNotification -> Bool)
                          -> Action (ResumableSource (ResourceT IO) PublicNotification)
listenFromWith route p = toJSONSource . responseBody <$> runGetRoute route
  where
    toJSONSource rs = rs $=+ (jsonConduit =$= CL.filter p)


-- | Transform the incoming bytestring stream representing a's in the form of
-- JSON into actual a's
jsonConduit :: (MonadThrow m, FromJSON a) => Conduit ByteString m a
jsonConduit = conduitParser jsonParser =$= CL.map snd

-- | An Attoparsec parser that attempts to parse the incoming bytestring in
-- JSON format.
jsonParser :: FromJSON a => AP.Parser ByteString a
jsonParser = value' >>= \v -> case fromJSON v of
                               Error s   -> fail s
                               Success x -> return x
             -- Note: we are using the json value parser here, since we want to
             --       be able to parse 'null' values (in case of Maybe ).


type FileTree = StorageTree FileName LastModificationTime (FileVersion ClientId)


getCurrentRealm   :: Path -> Action FileTree
getCurrentRealm p = do
    sync <- get
    resp <- runGetRoute $ CurrentRealmR (sync^.realm) p
    body <- bodyOf resp
    case eitherDecode body of
      Right t -> pure t
      Left  e -> print e           >> error "error"




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
    getFile'' resp fp


-- | Reads the actual file from the Response body, and saves it to disk
-- getFile''        :: Response -> FilePath -> Action ()
getFile''         ::  Response (ResumableSource (ResourceT IO) ByteString)
                  -> FilePath -> Action ()
getFile'' resp fp = do
      -- Download the file into a partial file
    let fpPartial = fp <.> partialFileExtension
    -- debugM "Actions.getFile" $ "Downloading into " <> show lpPartial
    lift $ responseBody resp $$+- sinkFile fpPartial
    -- debugM "Actions.getFile" $ "Download complete."
    -- debugM "Actions.getFile" "Moving partial file to actual path."
    liftIO $ renameFile fpPartial fp
    -- liftIO $ print "Should have renamed now "




--     -- set the modification time of the file we just downloaded
--     setModificationTimeBy s fp

-- setModificationTime      :: Signature -> FilePath -> Action ()
-- setModificationTime s fp = let t = toEpochTime . getDateTime $ fi in
--   toLocalPath p >>= (\fp -> liftIO $ setFileTimes (encodeString fp) t t )


downloadCurrent   :: Path -> Action ()
downloadCurrent p = do
    sync <- get
    resp <- runGetRoute $ DownloadCurrentR (sync^.realm) p
    case ( headerValue HFileKind $ responseHeaders resp
         , asLocalPath (sync^.config) p ) of
      (Just (File _), Just fp)  -> getFile'' resp fp
      (Just Directory, Just fp) -> do
                                     putStrLn "Unpacking dir"
                                     lift $ responseBody resp $$+- writeArchive fp
      (_, _)                    -> putStrLn "Error: No FileKind Header found"
      (_, Nothing)              -> putStrLn "Unknown path"



downloadVersion               :: FileKind -> Path -> Action ()
downloadVersion NonExistent _ = pure ()
downloadVersion Directory   _ = error "TODO"
downloadVersion (File s)    p = getFile s p

storeDirectory   :: Path -> Action ()
storeDirectory p = do
    sync <- get
    resp <- runPostRoute (CreateDirR (sync^.hsyncConfig.clientName) (sync^.realm) p)
                         mempty
    extractNotification resp






storeFile      :: FileKind -- ^ Signature of the file currently on the server
                           -- with this path
               -> Path -> Action ()
storeFile fk p = do
    sync <- get
    case asLocalPath (sync^.config) p of
      Nothing -> error "TODO: throw some exception or so"
      Just fp -> storeFile' fk p fp

-- | uploads the file pointed to by the FilePath to the remote Path (if the
-- file currently residing at the remote path has the geiven FileKind)
storeFile'         :: FileKind -> Path -> FilePath -> Action ()
storeFile' fk p fp = do
    sync <- get
    putStrLn "RUNNING Post"
    print fp
    let s = sourceFile fp :: Source (ResourceT IO) ByteString
    resp <- runPostRoute (StoreFileR (sync^.hsyncConfig.clientName)
                                     (sync^.realm) fk p)
                         s
    putStrLn "extracting resp"
    extractNotification resp


delete     :: FileKind -> Path -> Action ()
delete fk p = do
    sync <- get
    resp <- runDeleteRoute $ DeleteR (sync^.hsyncConfig.clientName) (sync^.realm) fk p
    extractNotification resp


extractNotification resp = do
    body <- bodyOf resp
    case eitherDecode body of
      Right (Right n) -> print (n :: PublicNotification)
      Right (Left e)  -> print (e :: Text)
      Left  e         -> print e
