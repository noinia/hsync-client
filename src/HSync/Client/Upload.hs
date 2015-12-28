module HSync.Client.Upload where

import Network
import HSync.Client.Import hiding (newManager)
import HSync.Client.Actions
import HSync.Client.Download
import Network.HTTP.Conduit(tlsManagerSettings, newManager)
import qualified System.Environment as Env
import System.Directory
import qualified Data.Conduit.Combinators as CC
import qualified Prelude as Prelude


uploadFile         :: FilePath -> FilePath -> Action ()
uploadFile root fp | traceShow (root,fp) False = undefined
uploadFile root fp = liftIO (makeAbsolute fp) >>= \fp' ->
                       case asRemotePathWith root fp' of
    Nothing -> let msg = "error determining path to upload '" <> fp <> "' to"
               in liftIO $ Prelude.putStrLn msg
    Just p  -> traceShow p $ storeFile NonExistent p

upload   :: FilePath -> Action ()
upload fp = do
    cwd <- liftIO getCurrentDirectory
    fs <- liftIO . runResourceT $ CC.sourceDirectoryDeep True fp $$ CC.sinkList
    mapM_ (uploadFile cwd) fs

mainWith [fp] = withSocketsDo $ getConfigs >>= \(hc,sc) -> withSync hc sc $ do
    b <- login
    if not b then
      liftIO $ putStrLn "Error authenticating"
    else do
      cwd <- liftIO getCurrentDirectory
      uploadFile cwd fp

main = Env.getArgs >>= mainWith
