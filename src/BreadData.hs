-- Example: IngredientRecord "Bread Flour" 100.0 "g"
type IngredientName = String
type Amount = Float
type Unit = String
data IngredientRecord = IngredientRecord IngredientName Amount Unit
                      deriving (Show, Eq)

scale :: IngredientRecord -> Float -> IngredientRecord
scale (IngredientRecord name amt unit) scaleFactor =
  (IngredientRecord name (amt * scaleFactor) unit)
