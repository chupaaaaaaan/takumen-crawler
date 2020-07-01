{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Concurrent
import           Data.IORef
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Client
import           System.Environment
import           Text.HTML.Scalpel
import qualified Web.Slack as Slack
import qualified Web.Slack.Chat as Slack

import Scraper
import Data
import Query
import AppFrame

prettyPrint :: Maybe [Item] -> IO ()
prettyPrint Nothing = T.putStrLn "No Content"
prettyPrint (Just item) = forM_ item $ \Item{..} ->
  T.putStrLn $ "{ " <> name <> "\n, " <> link <> "\n, " <> description <> "\n, " <> price <> "\n, " <> T.pack (show itemState) <> "\n} "

updateStore :: [Item] -> App ()
updateStore is = do
  storeRef <- asks store  
  liftIO $ writeIORef storeRef $ M.fromList $ map (\i -> (link i, i)) is

postItemMessages :: [Item] -> App ()
postItemMessages is = do
  channel  <- asks slackTargetChannel
  urls     <- asks urlConfig
  let msg = map (\i -> name i <> " is " <> T.pack (show $ itemState i) <> ".\n" <> baseUrl urls <> link i <> "\n" <> imgLink i) is
  forM_ msg $ \m -> Slack.chatPostMessage $ Slack.mkPostMsgReq channel m

listChangedTakumen :: [Item] -> App [Item]
listChangedTakumen is = do
  oldMap <- asks store >>= liftIO . readIORef
  igo <- forM is $ \item ->
    return $ case M.lookup (link item) oldMap of
               Nothing -> if itemState item == Available    then Just item else Nothing
               Just oi -> if itemState item /= itemState oi then Just item else Nothing
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
