{-# LANGUAGE RecordWildCards #-}
module Query where

import           Data.List

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

