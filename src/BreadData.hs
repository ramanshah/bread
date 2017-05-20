{-# LANGUAGE OverloadedStrings #-}

-- Types and parsers for reading a recipe into Haskell.

module BreadData where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Control.Applicative

-- Parse YAML to our data structure
yamlToBreadData :: String -> Either String [Section]
yamlToBreadData recipeYaml = Y.decodeEither $ BS.pack recipeYaml

data IngredientRecord =
  IngredientRecord {
    ingredientName :: String
  , amount :: Float
  , unit :: String
  } deriving (Eq, Show)

instance Y.FromJSON IngredientRecord where
  parseJSON (Y.Object v) =
    IngredientRecord <$>
    v .: "ingredient" <*>
    v .: "amount" <*>
    v .: "unit"

data Section =
  Section {
    sectionName :: String
  , ingredients :: [IngredientRecord]
  } deriving (Eq, Show)

instance Y.FromJSON Section where
  parseJSON (Y.Object v) =
    Section <$>
    v .: "section_name" <*>
    v .: "ingredients"
