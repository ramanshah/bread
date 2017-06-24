{-# LANGUAGE OverloadedStrings #-}

-- Rendering a bread recipe.

module Rendering where

import qualified Data.Ratio as R
import Formatting
import BreadData

type RecipeFieldLengths = (Int, Int, Int)

-- Render ingredient record into a line
render :: Int -> RecipeFieldLengths -> IngredientRecord -> String
render sigFigs (x, y, z) record =
    formatToString
      ((right x ' ' %. string) % " " %
       (left y ' ' %. string) % " " %
       (right z ' ' %. string))
      (ingredientName record)
      (renderAmount sigFigs $ amount record)
      (unit record)

-- Render section into a sequence of lines
renderSection :: Int -> RecipeFieldLengths -> Section -> [String]
renderSection sigFigs lengths (Section {sectionName = name, ingredients = xs}) =
  [dashes lengths, name, dashes lengths] ++
    map (render sigFigs lengths) xs

-- Render recipe into a sequence of lines
renderRecipe :: Int -> Recipe -> [String]
renderRecipe sigFigs sections =
  concatMap (renderSection sigFigs fieldLengths) sections ++
    [dashes fieldLengths]
      where fieldLengths = recipeFieldLengths sigFigs sections

-- Render numerical quantities to strings sensibly: one wants to render as few
-- significant digits as possible while keeping n significant digits of
-- accuracy. Rendering as few significant digits as possible is provided by the
-- Formatting library.
--
-- For three significant digits (renderAmount 3 x):
--      0.0     => "0"
--      23.4601 => "23.5"
--      1500    => "1500"
--      0.04659 => "0.0466"
renderAmount :: Int -> Float -> String
renderAmount _ 0.0 = "0"
renderAmount sigFigs x = formatToString shortest $ roundSigFig sigFigs x

-- Round to requested number of significant digits. If the number of
-- significant digits is greater than or equal to the place of the most
-- significant digit, move the decimal to the right the correct number of
-- places; round to an integer; then move it left. In the other case, move the
-- decimal to the left; round off; then move it right. Use the Rational type so
-- these rounded results come out exactly as rounded.
roundSigFig :: Int -> Float -> Rational
roundSigFig sigFigs x
  | k >= 0 = (round $ x * (10 ^ k)) R.% (10 ^ k)
  | k < 0  = (round $ x / (10.0 ^ (-k))) * (10 ^ (-k)) R.% 1
    where
      k = sigFigs - placeOfLeadingDigit
      placeOfLeadingDigit = (floor $ logBase 10 $ abs x) + 1

ingredientFieldLengths :: Int -> IngredientRecord -> RecipeFieldLengths
ingredientFieldLengths sigFigs IngredientRecord
  {ingredientName = i, amount = a, unit = u} =
    (length i, length $ renderAmount sigFigs a, length u)

maxFieldLengths :: [RecipeFieldLengths] -> RecipeFieldLengths
maxFieldLengths xs = (maximum a, maximum b, maximum c)
  where
    (a, b, c) = unzip3 xs

-- Field lengths implied by a section are defined as follows. First, size each
-- field so it's big enough to fit that field for all of the ingredients in the
-- section. But if the section name is too long to render that way, expand the
-- second (ingredient name) field so all of it fits.
sectionFieldLengths :: Int -> Section -> RecipeFieldLengths
sectionFieldLengths sigFigs Section {sectionName = n, ingredients = is} =
  if length n <= a + b + c + 2
     then (a, b, c)
     else (a, length n - a - c - 2, c)
  where
    (a, b, c) = maxFieldLengths $ map (ingredientFieldLengths sigFigs) is

recipeFieldLengths :: Int -> Recipe -> RecipeFieldLengths
recipeFieldLengths sigFigs r = maxFieldLengths $
  map (sectionFieldLengths sigFigs) r

-- To demarcate information in the rendered recipe
dashes :: RecipeFieldLengths -> String
dashes (x, y, z) = take (x + y + z + 2) $ repeat '-'
