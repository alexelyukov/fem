module Controllers.ElectrostaticFieldCordController (
  handler,
  url
) where

import Types
import Data.Aeson

handler :: Geometry -> String
handler geometry =
  let triangulation = [Triangle { a = Point { x = 200, y = 200 }, b = Point { x = 200, y = 300 }, c = Point { x = 300, y = 250 }}]
      voronoi = [[Point { x = 500, y = 500 }, Point { x = 500, y = 600 }, Point { x = 600, y = 550 }]]
  in show $ encode (TriangulationResponse {triangulation = triangulation, voronoi = voronoi})

url :: String
url = "electrostatic-field-cord-triangulation"
