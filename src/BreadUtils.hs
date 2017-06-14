-- Scaling and rendering a recipe.

module BreadUtils where

-- TODO: eliminate printf
import Text.Printf
import Data.Ratio
import Formatting
import qualified Data.Text.Lazy as L
import BreadData

type RecipeFieldLengths = [Int]

-- Scale the amount of an individual record.
scale :: Float -> IngredientRecord -> IngredientRecord
scale k (IngredientRecord {ingredientName = i, amount = a, unit = u}) =
  IngredientRecord {ingredientName = i, amount = k * a, unit = u}

-- Scale the amounts within a whole section.
scaleSection :: Float -> Section -> Section
scaleSection k (Section {sectionName = s, ingredients = i}) =
  Section {sectionName = s, ingredients = (map (scale k) i)}

-- Scale the amounts in a whole recipe.
scaleRecipe :: Float -> Recipe -> Recipe
scaleRecipe k sections = map (scaleSection k) sections

-- Render ingredient record into a line
render :: RecipeFieldLengths -> IngredientRecord -> String
render [x, y, z] record =
  printf formatString (ingredientName record)
    (renderAmount 3 $ amount record)
    (unit record)
    where
      formatString = "%-" ++ show x ++ "s %" ++
        show y ++ "s %-" ++ show z ++ "s"

-- Render section into a sequence of lines
renderSection :: RecipeFieldLengths -> Section -> [String]
renderSection lengths (Section {sectionName = name, ingredients = xs}) =
  [dashes lengths, name, dashes lengths] ++
    map (render lengths) xs

-- Render recipe into a sequence of lines
renderRecipe :: Recipe -> [String]
renderRecipe sections =
  concatMap (renderSection fieldLengths) sections ++
    [dashes fieldLengths]
      where fieldLengths = [28, 1, 3]

-- Render numerical quantities sensibly: one wants to render as few significant
-- digits as possible while keeping n significant digits of accuracy. The
-- Formatting library does the heavy lifting here, but we have to do the sigfig
-- computation.
-- e.g. for three significant digits:
--      0.0     => "0"
--      23.4601 => "23.5"
--      1500    => "1500"
--      0.04659 => "0.0466"
renderAmount :: Int -> Float -> String
renderAmount _ 0.0 = "0"
renderAmount sigFigs x = L.unpack $ format shortest $ roundSigFig sigFigs x

roundSigFig :: Int -> Float -> Rational
roundSigFig sigFigs x
  | k >= 0 = (round $ x * (10 ^ k)) Data.Ratio.% (10 ^ k)
  | k < 0  = (round $ x / (10.0 ^ (-k))) * (10 ^ (-k)) Data.Ratio.% 1
    where
      k = sigFigs - (floor $ logBase 10 $ abs x) - 1

-- To demarcate information in the rendered recipe
dashes :: RecipeFieldLengths -> String
dashes lengths = take (sum lengths + 2) $ repeat '-'
