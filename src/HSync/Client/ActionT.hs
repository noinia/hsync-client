module HSync.Client.ActionT where

import Prelude
import Yesod.Client
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control(MonadBaseControl)
import Control.Monad.Trans.Resource(MonadResource)
import Control.Monad.State.Class
import Control.Monad.State( MonadState(..)
                          , StateT(..)
                          , runStateT
                          , modify
                          )
import Yesod.Core

newtype ActionT sync m a = ActionT {
                             runActionT :: StateT sync (YesodClientT sync m) a }
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
