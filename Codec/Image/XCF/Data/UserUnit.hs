module Codec.Image.XCF.Data.UserUnit
       (UserUnit (..))
       where

import Data.Text

data UserUnit = UserUnit {
  factor :: Float,
  numDecimals :: Int,
  identifier :: Text,
  symbol :: Text,
  abbreviation :: Text,
  nameSingular :: Text,
  namePlural :: Text
  } deriving Show