module Controllers.ElectrostaticFieldCordController (
  handler,
  url
) where

import Types

handler :: Geometry -> String
handler geometry = show geometry

url :: String
url = "electrostatic-field-cord-triangulation"
