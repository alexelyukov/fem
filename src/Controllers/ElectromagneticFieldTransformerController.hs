module Controllers.ElectromagneticFieldTransformerController (
  handler,
  url
) where

import Types

handler :: Geometry -> String
handler geometry = show geometry

url :: String
url = "electromagnetic-field-transformer-triangulation"
