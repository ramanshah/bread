{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import qualified Data.Yaml as Y
import System.Environment
import System.Exit
import Text.Read

import BreadData
import Scaling
import Rendering

-- Error-check scale factor
readScaleFactor :: String -> Either String Float
readScaleFactor s = case result of
  Left s -> Left msg
  Right x -> if (x >= 0.0)
               then Right x
               else Left msg
  where result = readEither s :: Either String Float
        msg = "Scale factor must be a non-negative floating point number."

-- Provide unified way to deal with bad input via the Either monad
printIfError :: Either String a -> IO a
printIfError (Left s) = do
  putStrLn s
  exitWith (ExitFailure 1)
printIfError (Right x) = return x

-- Error-check argument list
checkArgLength :: [String] -> Int -> IO ()
checkArgLength as x
  | length as == x = return ()
  | otherwise = do
      putStrLn "Usage: bread path/to/recipe.yml scale_factor"
      exitWith (ExitFailure 1)

main :: IO ()
main = do
  -- Read in command-line arguments: source YAML and scale factor
  args <- getArgs
  checkArgLength args 2
  recipeYaml <- readFile $ args !! 0
  scaleFactor <- printIfError $ readScaleFactor $ args !! 1

  recipe <- printIfError $ yamlToBreadData recipeYaml
  mapM_ putStrLn $ renderRecipe 3 $ scaleRecipe scaleFactor recipe
  exitWith ExitSuccess
