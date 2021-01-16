module Controllers.ElectrodynamicSystemController (
  handler,
  url
) where

import Types

handler :: Geometry -> String
handler geometry = show geometry

url :: String
url = "electrodynamic-system-triangulation"
