{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Network.HTTP.Client
import           Text.HTML.Scalpel
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.ICU                 as TI
import Control.Monad
import Control.Monad.Reader 
import Control.Monad.Trans.Reader hiding (ask, asks)
import Data.IORef

type Name = Text
type Link = Text
type Description = Text
type Price = Text
data ItemState = SoldOut | Available

data Item = Item { name        :: Name
                 , link        :: Link
                 , description :: Description
                 , price       :: Price
                 , itemState   :: ItemState
                 }

prettyPrint :: Maybe [Item] -> IO ()
prettyPrint Nothing = T.putStrLn "No Content"
prettyPrint (Just items) = forM_ items $ \Item{..} -> do
  case itemState of
    SoldOut   -> T.putStrLn $ "{ " <> name <> "\n, " <> link <> "\n, " <> rmLn description <> "\n, " <> rmLn price <> "\n, " <> "SoldOut"   <> "\n} "
    Available -> T.putStrLn $ "{ " <> name <> "\n, " <> link <> "\n, " <> rmLn description <> "\n, " <> rmLn price <> "\n, " <> "Available" <> "\n} "
  where rmLn = T.concat . T.lines


data Env = Env { store :: IORef Text
               , manager :: Manager
               }

-- newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

newtype App a = App (ReaderT Env IO a)
  deriving( Functor
          , Applicative
          , Monad
          , MonadReader Env
          , MonadIO
          )

runApp :: Env -> App a -> IO a
runApp env (App m) = runReaderT m env

baseUrl = "https://www.takumen.com"
searchUrl = baseUrl <> "/search/index?all=true"
soldOutImgUrl = "/assets/btn_soldout_mini.jpg"

items :: Scraper Text [Item]
items = chroots ("div" @: ["id" @= "products"] // "div" @: [hasClass "inner", hasClass "clearfix"]) $ do
  n  <- text        $ "div" @: [hasClass "textBox"]  // "p" @: [hasClass "pLink"] // "a"
  l  <- attr "href" $ "div" @: [hasClass "textBox"]  // "p" @: [hasClass "pLink"] // "a"
  d  <- text        $ "div" @: [hasClass "textBox"]  // "p" @: [hasClass "title"]
  p  <- text        $ "div" @: [hasClass "textBox"]  // "p" @: [hasClass "price"]
  s' <- attr "src"  $ "div" @: [hasClass "checkBox"] // "img"

  let s = if s' == soldOutImgUrl then SoldOut else Available
  return $ Item n l d p s



app :: App ()
app = do
  ref <- asks store
  manager <- liftIO $ newManager defaultManagerSettings
  liftIO $ scrapeURLWithConfig (Config utf8Decoder Nothing) searchUrl items >>= prettyPrint
  -- forever $ do
  --   result <- liftIO $ scrapeURLWithConfig (Config utf8Decoder Nothing) searchUrl items
  --   case result of
  --     Nothing -> return ()
  --     Just x -> do
  --       old <- liftIO $ readIORef ref

  --       return ()
  
  return ()


main :: IO ()
main = do
  s <- newIORef T.empty
  m <- newManager defaultManagerSettings
  runApp (Env s m) app
