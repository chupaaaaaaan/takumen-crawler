{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Concurrent
import           Control.Monad.Cont
import           Control.Monad.Reader
import           Data.Aeson
import           Data.IORef
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Generics (Generic)
import           Network.HTTP.Client
import           System.Environment
import           Text.HTML.Scalpel
import qualified Web.Slack as Slack
import qualified Web.Slack.Chat as Slack
import qualified Web.Slack.Common as Slack

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
  T.putStrLn $ "{ " <> name <> "\n, " <> link <> "\n, " <> description <> "\n, " <> price <> "\n, " <> (T.pack $ show $ itemState) <> "\n} "


data RamenQuery = All | Condition { category :: Maybe Category
                                  , taste :: Maybe Taste
                                  , soup :: Maybe Soup
                                  , intensity :: Maybe Intensity
                                  , noodle :: Maybe Noodle
                                  }
instance Show RamenQuery where
  show All = "all=true"
  show Condition{..} = concat . ("?":) . intersperse "&" $ [ qq category, qq taste, qq soup, qq intensity, qq noodle]
    where qq (Just x) = show x
          qq Nothing = ""

data Category = Ramen | Tsukemen | Soupless
instance Show Category where
  show Ramen = "category[1]"
  show Tsukemen = "category[2]"
  show Soupless = "category[3]"
data Taste = SuperLight | Light | TMedium | Heavy | SuperHeavy
instance Show Taste where
  show SuperLight = "taste[1]"
  show Light = "taste[2]"
  show TMedium = "taste[3]"
  show Heavy = "taste[4]"
  show SuperHeavy = "taste[5]"
data Soup = Shoyu | Miso | Shio | Tonkotsu | Inspire | Tantan | GyokaiTonkotsu | ToriPaitan | Other
instance Show Soup where
  show Shoyu = "soup[1]"
  show Miso = "soup[2]"
  show Shio = "soup[3]"
  show Tonkotsu = "soup[4]"
  show Inspire = "soup[5]"
  show Tantan = "soup[6]"
  show GyokaiTonkotsu = "soup[8]"
  show ToriPaitan = "soup[9]"
  show Other = "soup[7]"
data Intensity = SuperWeek | Week | IMedium | Strong | SuperStrong
instance Show Intensity where
  show SuperWeek = "density[1]"
  show Week = "density[2]"
  show IMedium = "density[3]"
  show Strong = "density[4]"
  show SuperStrong = "density[5]"
data Noodle = SuperThin | Thin | NMedium | Thick | SuperThick
instance Show Noodle where
  show SuperThin = "noodle[1]"
  show Thin = "noodle[2]"
  show NMedium = "noodle[3]"
  show Thick = "noodle[4]"
  show SuperThick = "noodle[5]"

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

updateStore :: [Item] -> App ()
updateStore is = do
  storeRef <- asks store  
  liftIO $ writeIORef storeRef $ M.fromList $ map (\i -> (link i, i)) is

postItemMessages :: [Item] -> App ()
postItemMessages is = do
  channel  <- asks slackTargetChannel
  urls     <- asks urlConfig
  let msg = map (\i -> name i <> " is " <> (T.pack $ show $ itemState i) <> ".\n" <> baseUrl urls <> link i <> "\n" <> imgLink i) is
  forM_ msg $ \m -> Slack.chatPostMessage $ Slack.mkPostMsgReq channel m

listChangedTakumen :: [Item] -> App [Item]
listChangedTakumen is = do
  oldMap <- asks store >>= liftIO . readIORef
  igo <- forM is $ \item ->
    return $ case M.lookup (link item) oldMap of
               Nothing -> if itemState item == Available    then Just item else Nothing
               Just oi -> if itemState item == itemState oi then Just item else Nothing
  return $ map fromJust . filter (/=Nothing) $ igo

app :: App ()
app = do
  urls <- asks urlConfig
  forever $ do
    scraped <- liftIO $ scrapeURLWithConfig (Config utf8Decoder Nothing) (T.unpack $ baseUrl urls <> searchUrl urls) items

    -- liftIO $ prettyPrint scraped

    case scraped of
      Nothing -> error "結果の取得に失敗しました。"
      Just result -> do
        listChangedTakumen result >>= postItemMessages
        updateStore result

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
