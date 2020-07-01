{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AppFrame where

import Control.Monad.Reader
import Data.IORef
import Data.Map.Strict (Map)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Web.Slack
import Data

data UrlConfig = UrlConfig { baseUrl :: Text
                           , searchUrl :: Text
                           }

data Env = Env { store :: IORef (Map Link Item)
               , manager :: Manager
               , urlConfig :: UrlConfig
               , slackConfig :: SlackConfig
               , slackTargetChannel :: Text
               }

instance HasManager Env where
  getManager = slackConfigManager . slackConfig

instance HasToken Env where
  getToken = slackConfigToken . slackConfig

newtype App a = App (ReaderT Env IO a)
  deriving( Functor
          , Applicative
          , Monad
          , MonadReader Env
          , MonadIO
          )

runApp :: Env -> App a -> IO a
runApp env (App m) = runReaderT m env
