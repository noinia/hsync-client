module HSync.Client.ServerState where

import Prelude
import HSync.Common.Types
import HSync.Common.Notification
import HSync.Common.FileVersion
import Data.Default
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Lens
import Data.Acid(Query, Update, makeAcidic, liftQuery)
import Data.SafeCopy(base, deriveSafeCopy)
import HSync.Client.Types
import qualified HSync.Common.StorageTree as ST

--------------------------------------------------------------------------------

data ServerState = ServerState { _realmState     :: Maybe FileTree
                               , _storedClientId :: Maybe ClientId
                               }
                   deriving (Show,Read)

$(deriveSafeCopy 0 'base ''ServerState)
makeLenses ''ServerState


instance Default ServerState where
  def = ServerState Nothing (Just (ClientId 1)) -- TODO, fix the clientID thing


queryClientId :: Query ServerState (Maybe ClientId)
queryClientId = (^.storedClientId) <$> ask

setClientId    :: ClientId -> Update ServerState ()
setClientId ci = modify (&storedClientId .~ Just ci)


queryRealmState :: Query ServerState (Maybe FileTree)
queryRealmState = (^.realmState) <$> ask

access          :: Path -> Query ServerState (Maybe FileTree)
access (Path p) = (>>= ST.access p) <$> queryRealmState

replaceRealmState    :: FileTree -> Update ServerState ()
replaceRealmState ft = modify (&realmState .~ Just ft)

updateByNotification   :: PublicNotification -> Update ServerState ()
updateByNotification n = modify (&realmState._Just %~ updateTree n)



updateTree   :: PublicNotification -> FileTree -> FileTree
updateTree n = ST.updateAt (n^.event.affectedPath.pathParts) (const v) parentData
  where
    v          = n^.event.newVersion
    parentData = FileVersion Directory (v^.lastModified) True





$(makeAcidic ''ServerState [ 'queryRealmState
                           , 'queryClientId
                           , 'setClientId
                           , 'access
                           , 'replaceRealmState
                           , 'updateByNotification
                           ])
