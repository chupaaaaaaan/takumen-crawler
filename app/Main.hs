{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Network.HTTP.Client
import Text.HTML.Scalpel
import Data.Aeson
import Data.Text (Text)
import Data.Maybe
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Reader
import Control.Concurrent
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Control.Monad.Cont
import qualified Web.Slack as Slack
import qualified Web.Slack.Chat as Slack
import qualified Web.Slack.Common as Slack
import System.Environment
import System.Random.MWC

type Name = Text
type Link = Text
type Description = Text
type Price = Text
data ItemState = SoldOut | Available deriving (Show, Eq, Generic)
instance ToJSON ItemState
instance FromJSON ItemState

data Item = Item { name        :: Name
                 , link        :: Link
                 , imgLink     :: Link
                 , description :: Description
                 , price       :: Price
                 , itemState   :: ItemState
                 } deriving (Show, Eq, Generic)

instance ToJSON Item
instance FromJSON Item

data UrlConfig = UrlConfig { baseUrl :: Text
                           , searchUrl :: Text
                           }

prettyPrint :: Maybe [Item] -> IO ()
prettyPrint Nothing = T.putStrLn "No Content"
prettyPrint (Just item) = forM_ item $ \Item{..} ->
  case itemState of
    SoldOut   -> T.putStrLn $ "{ " <> name <> "\n, " <> link <> "\n, " <> description <> "\n, " <> price <> "\n, " <> "SoldOut"   <> "\n} "
    Available -> T.putStrLn $ "{ " <> name <> "\n, " <> link <> "\n, " <> description <> "\n, " <> price <> "\n, " <> "Available" <> "\n} "


-- scraper
items :: Scraper Text [Item]
items = chroots ("div" @: ["id" @= "products"] // "div" @: [hasClass "inner", hasClass "clearfix"]) scrapeItem

scrapeItem :: Scraper Text Item
scrapeItem = do
  n <- scrapeName
  l <- scrapeLink
  i <- scrapeImgLink
  d <- scrapeDesc
  p <- scrapePrice
  s <- scrapeItemState
  return $ Item n l i d p s

scrapeName :: Scraper Text Name
scrapeName = text $ "div" @: [hasClass "textBox"]  // "p" @: [hasClass "pLink"] // "a"

scrapeLink :: Scraper Text Link
scrapeLink = attr "href" $ "div" @: [hasClass "textBox"]  // "p" @: [hasClass "pLink"] // "a"

scrapeImgLink :: Scraper Text Link
scrapeImgLink = do
  i <- attr "src" $ "div" @: [hasClass "photoBox"]  // "img"
  return $ "https:" <> i

scrapeDesc :: Scraper Text Description
scrapeDesc = do
  d <- text $ "div" @: [hasClass "textBox"]  // "p" @: [hasClass "title"]
  return . T.concat . T.lines $ d

scrapePrice :: Scraper Text Price
scrapePrice = do
  p <- text $ "div" @: [hasClass "textBox"]  // "p" @: [hasClass "price"]
  return . T.concat . T.lines $ p

scrapeItemState :: Scraper Text ItemState
scrapeItemState = do
  let soldOutImgUrl = "/assets/btn_soldout_mini.jpg"
  s <- attr "src"  $ "div" @: [hasClass "checkBox"] // "img"
  return $ if s == soldOutImgUrl then SoldOut else Available


-- app framework
data Env = Env { store :: IORef (Map Link Item)
               , manager :: Manager
               , urlConfig :: UrlConfig
               , slackConfig :: Slack.SlackConfig
               , slackTargetChannel :: Text
               }

instance Slack.HasManager Env where
  getManager = Slack.slackConfigManager . slackConfig

instance Slack.HasToken Env where
  getToken = Slack.slackConfigToken . slackConfig

newtype App a = App (ReaderT Env IO a)
  deriving( Functor
          , Applicative
          , Monad
          , MonadReader Env
          , MonadIO
          )

runApp :: Env -> App a -> IO a
runApp env (App m) = runReaderT m env

postAvailableTakumen :: [Item] -> App ()
postAvailableTakumen is = do
  storeRef <- asks store
  oldMap   <- liftIO $ readIORef storeRef
  channel  <- asks slackTargetChannel
  urls     <- asks urlConfig
  igo <- forM is $ \item ->
    return $ if itemState item == SoldOut
             then Nothing
             else case M.lookup (link item) oldMap of
                    Nothing -> Just item
                    Just oi -> if itemState oi == SoldOut then Just item else Nothing

  let msg = map ((\i -> name i <> "\n" <> baseUrl urls <> link i <> "\n" <> imgLink i) . fromJust) . filter (/=Nothing) $ igo

  forM_ msg $ \m -> Slack.chatPostMessage $ Slack.mkPostMsgReq channel m

  liftIO $ writeIORef storeRef $ M.fromList $ map (\i -> (link i, i)) is

app :: App ()
app = do
  -- gen <- liftIO $ createSystemRandom
  urls <- asks urlConfig
  forever $ do
    scraped <- liftIO $ scrapeURLWithConfig (Config utf8Decoder Nothing) (T.unpack $ baseUrl urls <> searchUrl urls) items

    liftIO $ prettyPrint scraped

    case scraped of
      Nothing -> error "結果の取得に失敗しました。"
      Just result -> postAvailableTakumen result

    liftIO $ threadDelay (60 * 1000000)

main :: IO ()
main = do
  [slackToken, slackTargetChannel'] <- getArgs
  storeRef <- newIORef M.empty
  manager' <- newManager defaultManagerSettings
  slackConfig' <- Slack.mkSlackConfig $ T.pack slackToken

  let urlConfig' = UrlConfig "https://www.takumen.com" "/search/index?all=true"
      env = Env storeRef manager' urlConfig' slackConfig' (T.pack slackTargetChannel')

  runApp env app
