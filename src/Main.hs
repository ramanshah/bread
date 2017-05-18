{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import BreadData

main :: IO ()
main = do
  x <- readFile("./doc/examples/ciabatta.yml")
  print $ ((Y.decodeEither $ BS.pack x) :: Either String [Section])
