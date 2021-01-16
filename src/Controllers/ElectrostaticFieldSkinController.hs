module Controllers.ElectrostaticFieldSkinController (
  handler,
  url
) where

import Types

handler :: Geometry -> String
handler geometry = show geometry

url :: String
url = "electrostatic-field-skin-triangulation"
