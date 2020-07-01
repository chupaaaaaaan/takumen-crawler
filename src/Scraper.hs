{-# LANGUAGE OverloadedStrings #-}
module Scraper where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.HTML.Scalpel.Core

import Data

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
