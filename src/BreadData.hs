{-# LANGUAGE OverloadedStrings #-}

-- Types and parsers for reading a recipe into Haskell.

module BreadData where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import qualified Data.Aeson.Types as A
import Data.Aeson.Types ((.=))
import Data.Monoid ((<>))
import Control.Applicative

-- Parse YAML to our data structure
yamlToBreadData :: String -> Either String Recipe
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

instance Y.ToJSON IngredientRecord where
  toJSON v = Y.object $ [
    "ingredient" .= ingredientName v,
    "amount" .= amount v,
    "unit" .= unit v]

  toEncoding v = A.pairs $
    "ingredient" .= ingredientName v <>
    "amount" .= amount v <>
    "unit" .= unit v

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

instance Y.ToJSON Section where
  toJSON v = Y.object $ [
    "section_name" .= sectionName v,
    "ingredients" .= ingredients v]

  toEncoding v = A.pairs $
    "section_name" .= sectionName v <>
    "ingredients" .= ingredients v

type Recipe = [Section]
