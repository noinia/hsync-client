module HSync.Client.ServerState where

import HSync.Client.Import.NoFoundation
import Data.Acid(Query, Update, makeAcidic, liftQuery)
import Data.SafeCopy(base, deriveSafeCopy)


import qualified Data.Map  as M


--------------------------------------------------------------------------------



data ServerState = ServerState { _dummy :: Int }
                   deriving (Show,Read)

$(deriveSafeCopy 0 'base ''ServerState)
makeLenses ''ServerState
