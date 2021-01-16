module Controllers.ThermalFieldTubeController (
  handler,
  url
) where

import Types

handler :: Geometry -> String
handler geometry = show geometry

url :: String
url = "thermal-field-tube-triangulation"
