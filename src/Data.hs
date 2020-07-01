{-# LANGUAGE DeriveGeneric #-}
module Data where

import           GHC.Generics (Generic)
import           Data.Aeson
import           Data.Text (Text)

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
