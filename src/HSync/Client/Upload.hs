module HSync.Client.Upload where

import HSync.Client.Actions
import HSync.Client.Import
import qualified System.Environment as Env
import System.Directory
import qualified Prelude as Prelude
import qualified Data.Conduit.Combinators as CC
import qualified Network.HTTP.Conduit as HC









uploadFile         :: FilePath -> FilePath -> Action ()
uploadFile root fp = liftIO (makeAbsolute fp) >>= \fp' ->
                       case asRemotePathWith root fp' of
    Nothing -> let msg = "error determining path to upload '" <> fp <> "' to"
               in liftIO $ Prelude.putStrLn msg
    Just p  -> storeFile' NonExistent p fp

upload    :: FilePath -> Action ()
upload fp = do
    cwd <- liftIO getCurrentDirectory
    fs <- liftIO . runResourceT $ CC.sourceDirectoryDeep True fp $$ CC.sinkList
    mapM_ (uploadFile cwd) fs

mainWith      :: [String] -> IO ()
mainWith [fp] = runMain $ do
    b <- login
    if not b then
      liftIO $ putStrLn "Error authenticating"
    else do
      -- cwd <- liftIO getCurrentDirectory
      upload fp

main :: IO ()
main = Env.getArgs >>= mainWith
