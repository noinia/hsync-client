module HSync.Client.Download where

import Network
import HSync.Client.Import hiding (newManager)
import HSync.Client.Actions
import HSync.Client.Foundation
import System.Environment

main = runMain $ do
    b <- login
    liftIO $ print b
    -- storeDirectory (Path ["foo"])
    tr <- getCurrentRealm (Path [])
    liftIO $ print tr
    downloadCurrent (Path [])
