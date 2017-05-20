{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import System.Environment
import BreadData
import BreadUtils

main :: IO ()
main = do
  -- Read in command-line arguments: source YAML and scale factor
  args <- getArgs
  recipeYaml <- readFile(args !! 0)
  scaleFactor <- return (read (args !! 1) :: Float)

  parsed <- return ((Y.decodeEither $
    BS.pack recipeYaml) :: Either String [Section])
  case parsed of
    Left s -> putStrLn $ "Parse error: " ++ s
    Right recipe -> do
      print $ scaleRecipe scaleFactor recipe
