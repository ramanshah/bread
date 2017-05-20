{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import System.Environment
import System.Exit

import BreadData
import BreadUtils

-- Provide unified way to deal with bad input via the Either monad
printIfError :: Either String a -> IO a
printIfError (Left s) = do
  putStrLn s
  exitWith (ExitFailure 1)
printIfError (Right x) = return x

main :: IO ()
main = do
  -- Read in command-line arguments: source YAML and scale factor
  args <- getArgs
  recipeYaml <- readFile $ args !! 0
  scaleFactor <- return (read (args !! 1) :: Float)

  recipe <- printIfError ((Y.decodeEither $
    BS.pack recipeYaml) :: Either String [Section])

  print $ scaleRecipe scaleFactor recipe
  exitWith ExitSuccess
