{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Options.Applicative (execParser)
import qualified Data.Yaml as Y
import System.Environment
import System.Exit
import Text.Read

import BreadData
import Scaling
import Rendering
import Cli

-- Error-check scale factor
checkScaleFactor :: Float -> Either String Float
checkScaleFactor x =
  if x >= 0.0
    then Right x
    else Left msg
  where
    msg = "Scale factor must be a non-negative floating point number."

-- Provide unified way to deal with bad input via the Either monad
printIfError :: Either String a -> IO a
printIfError (Left s) = do
  putStrLn s
  exitWith (ExitFailure 1)
printIfError (Right x) = return x

main :: IO ()
main = do
  -- Read in command-line arguments: source YAML and scale factor
  args <- execParser cliOpts
  recipeYaml <- readFile $ filePath args
  scaleFactor <- printIfError $ checkScaleFactor $ scaleFactor args

  recipe <- printIfError $ yamlToBreadData recipeYaml
  mapM_ putStrLn $ renderRecipe 3 $ scaleRecipe scaleFactor recipe
  exitWith ExitSuccess
