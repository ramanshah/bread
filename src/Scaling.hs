-- Scaling a recipe.

module Scaling where

import BreadData

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
