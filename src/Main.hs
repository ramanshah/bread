{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import BreadData
import BreadUtils

main :: IO ()
main = do
  x <- readFile("./doc/examples/ciabatta.yml")
  parsed <- return ((Y.decodeEither $ BS.pack x) :: Either String [Section])
  case parsed of
    Left s -> putStrLn $ "Parse error: " ++ s
    Right recipe -> do
      print $ scaleRecipe 1.2 recipe
