module HSync.Client.ActionT where

import Prelude
import Data.Default
import Yesod.Client
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control(MonadBaseControl)
import Control.Monad.Trans.Resource(MonadResource, ResourceT, runResourceT)
import Control.Monad.State.Class
import Control.Monad.State( MonadState(..)
                          , StateT(..)
                          , evalStateT
                          , modify
                          )
import Yesod.Core

newtype ActionT sync m a = ActionT {
                             unActionT :: StateT sync (YesodClientT sync m) a }
                         deriving ( Functor,Applicative,Monad,MonadIO
                                  , MonadState sync)

instance MonadTrans (ActionT sync) where
  lift = ActionT . lift . lift

instance ( MonadResource m
         , MonadBaseControl IO m
         , RenderRoute (API sync)
         , IsYesodClient sync
         ) =>
         MonadYesodClient sync (ActionT sync) m where
  runGetRoute r     = liftYT $ runGetRoute r
  runPostRoute r s  = liftYT $ runPostRoute r s
  runDeleteRoute r  = liftYT $ runDeleteRoute r

-- | Lift a YesodClientT m action into a ActionT m action
liftYT :: Monad m => YesodClientT sync m a -> ActionT sync m a
liftYT = ActionT . lift

runActionT     :: MonadBaseControl IO m => ActionT sync (ResourceT m) a -> sync -> m a
runActionT act = runResourceT . runActionT' act

runActionT'       :: Monad m => ActionT sync m a -> sync -> m a
runActionT' act s = evalYesodClientT (evalStateT (unActionT act) s) s def
