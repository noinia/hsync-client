module HSync.Client.AcidState where


import Prelude
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans(MonadIO(..))
import Data.Default
import HSync.Common.AcidState

import HSync.Client.ServerState
import HSync.Common.Types

import Data.Acid(AcidState(..))
import Control.Lens


data Acids = Acids { _serverState :: AcidState ServerState }
makeLenses ''Acids

-- instance Default Acids where
--   def = Acids def


withAcids            :: ( MonadBaseControl IO m
                        , MonadIO m
                        ) => Maybe FilePath -> (Acids -> m a) -> m a
withAcids stateDir f = withAcidState stateDir def $ \ss ->
                           f (Acids ss)
