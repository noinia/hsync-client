{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
module Yesod.Client where

import Prelude
import Control.Applicative((<$>))

import Blaze.ByteString.Builder( -- Builder
                               -- , fromByteString
                               -- , flush
                                toLazyByteString
                               )


import Control.Monad.Reader(MonadReader(..),ReaderT,runReaderT)
import Control.Monad.State( MonadState(..)
                          , StateT(..)
                          , runStateT
                          , modify
                          )

import Control.Monad.Catch(MonadThrow(..))
import Control.Monad.Trans.Resource(ResourceT)

import Data.ByteString(ByteString, empty)

import Data.Conduit(Source, ResumableSource, mapOutput, transPipe)
import Data.Default
import Data.Text(Text)




import Network.HTTP.Conduit( Request
                           , Response
                           , Manager
                           , CookieJar
                           , RequestBody(..)
                           -- , HttpException(..)
                           , http
                           , parseUrl
                           , createCookieJar
                           -- , withManager
                           , method
                           , requestBody
                           , requestBodySourceChunked
                           , responseCookieJar
                           )

import Network.HTTP.Types

import Yesod.Core

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Network.HTTP.Conduit       as HC

import qualified Network.HTTP.Types.Header  as H


--------------------------------------------------------------------------------

-- | Specifies what type of operations a yesodClient should support
class IsYesodClient client where
    type API client

    serverAppRoot  :: client -> Text
    manager        :: client -> Manager
    api            :: client -> API client

    defaultRequestModifier   :: client -> Request -> Request
    defaultRequestModifier _ = id


--------------------------------------------------------------------------------
-- *

-- | Dummy Yesod-server to generate the URL's for our api
data YesodServer api = YesodServer { serverRoot :: !Text
                                   , serverApi  :: api
                                   }

instance Eq (Route api) => Eq (Route (YesodServer api)) where
  (ServerRoute r) == (ServerRoute r') = r == r'

instance RenderRoute api => RenderRoute (YesodServer api) where
  data Route (YesodServer api) = ServerRoute (Route api)
  renderRoute (ServerRoute r) = renderRoute r

instance RenderRoute api => Yesod (YesodServer api) where
  approot = ApprootMaster serverRoot



-- | Given a client, get me a YesodServer serving the API
server     :: IsYesodClient cli => cli -> YesodServer (API cli)
server cli = YesodServer (serverAppRoot cli) (api cli)


--------------------------------------------------------------------------------
-- | Basic actions that we can run on something that is a MonadYesodClient

class ( MonadResource m
      , MonadBaseControl IO m
      , IsYesodClient client
      , RenderRoute (API client)
      ) =>
      MonadYesodClient client yt m | yt -> client where

  runGetRoute :: Route (API client) ->
                 yt m (Response (ResumableSource m ByteString))

  runPostRoute :: Route (API client) ->
                   Source (ResourceT IO) ByteString ->
                   yt m (Response (ResumableSource m ByteString))


  runDeleteRoute :: Route (API client) ->
                     yt m (Response (ResumableSource m ByteString))




--------------------------------------------------------------------------------

-- | A monad transformer that implements the MonadYesodClient actions
newtype YesodClientT cli m a =
    YesodClientT { unYCT :: StateT YesodClientState (ReaderT cli m) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadThrow)

-- | .. a monad transformer,
instance MonadTrans (YesodClientT cli) where
    lift = YesodClientT . lift . lift

-- | a reader monad.
instance Monad m => MonadReader cli (YesodClientT cli m) where
    ask     = YesodClientT . lift $ ask
    local f = YesodClientT . StateT . fmap (local f) . runStateT . unYCT

--------------------------------------------------------------------------------

-- | The state maintained by a YesodClient
data YesodClientState = YesodClientState { cookieJar'      :: Maybe CookieJar
                                         }
                         deriving (Show,Eq)

instance Default YesodClientState where
    def = YesodClientState . Just . createCookieJar $ []

-- | YesodClientT maintains YesodClientState as state
instance Monad m => MonadState YesodClientState (YesodClientT cli m) where
    state = YesodClientT . state

--------------------------------------------------------------------------------

-- | Run a yesodClientT monad transformer
runYesodClientT                                :: YesodClientT cli m a ->
                                                  cli ->
                                                  YesodClientState ->
                                                      m (a,YesodClientState)
runYesodClientT (YesodClientT comp) cli s = runReaderT (runStateT comp s) cli


-- | run an action, then drop the client state
evalYesodClientT            :: Functor m => YesodClientT cli m a ->
                                 cli -> YesodClientState -> m a
evalYesodClientT comp cli s = fst <$> runYesodClientT comp cli s


-- | Get the client instance
clientInstance :: Monad m => YesodClientT cli m cli
clientInstance = ask

-- | Get the cookiejar
cookieJar :: Monad m => YesodClientT cli m (Maybe CookieJar)
cookieJar = cookieJar' <$> get


-- | Update the cookiejar
updateCookieJar   :: Monad m => Response body -> YesodClientT cli m ()
updateCookieJar r = modify $ \st -> st {cookieJar' = Just . responseCookieJar $ r}

--------------------------------------------------------------------------------

-- | The MonadYesodClient instance for the YesodClientT
instance ( MonadResource m
         , MonadBaseControl IO m
         , IsYesodClient client
         , RenderRoute (API client)
         ) =>
         MonadYesodClient client (YesodClientT client) m where

  runGetRoute = flip runRouteWith id

  runPostRoute r s = runRouteWith r $ \req ->
                       req { method      = methodPost
                           , requestBody = requestBodySourceChunked s
                           }

  runDeleteRoute r = runRouteWith r $ \req ->
                       req { method      = methodDelete
                           , requestBody = RequestBodyBS empty
                           }

--------------------------------------------------------------------------------
-- | Relating the Client to the server

type IsYesodClientFor client api = ( IsYesodClient client
                                   , RenderRoute api
                                   , API client ~ api
                                   )


toUrl     :: (client `IsYesodClientFor` api) =>
             client -> Route api -> String
toUrl cli r = let root     = serverAppRoot cli
                  (pcs,qs) = renderRoute r
                  urlBldr  = joinPath (server cli) root pcs qs in
              LC.unpack . toLazyByteString $ urlBldr



toReq   :: (client `IsYesodClientFor` api,
            Functor m,
            MonadThrow m ) =>
           Route api -> YesodClientT client m Request
toReq r = do
  cli <- clientInstance
  mcj <- cookieJar
  req <- parseUrl . toUrl cli $ r
  return . defaultRequestModifier cli $ req { HC.cookieJar       = mcj
                                            , HC.responseTimeout = Nothing
                                            }


-- | Given a route and a request modification function f. Create a request for
-- this route, modify it with f, and then run the request.
runRouteWith   :: ( client `IsYesodClientFor` api
               , MonadResource m, MonadBaseControl IO m
               ) =>
               Route api ->
               (Request -> Request) ->
                   YesodClientT client m (Response (ResumableSource m ByteString))
runRouteWith r f = do
  mgr <- manager <$> clientInstance
  req <- f <$> toReq r
  lift $ http req mgr



addRequestHeader         :: H.Header -> Request -> Request
addRequestHeader hdr req = req { HC.requestHeaders = hdr : HC.requestHeaders req }
