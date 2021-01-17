module Controllers.ElectromagneticFieldTransformerController (
  handler,
  url
) where

import Types
import Data.Aeson
import System.Random
import Generator

handler :: Geometry -> String
handler geometry =
  let triangulation = getTriangulation geometry
      voronoi = getVoronoi geometry
  in show $ encode (TriangulationResponse {triangulation = triangulation, voronoi = voronoi})

url :: String
url = "electromagnetic-field-transformer-triangulation"

getTriangulation :: Geometry -> Triangulation
getTriangulation geometry =
  let generator = mkStdGen 10
      (generatedPoints, generator') = generatePoints generator geometry 3000
      triangulationTest = map (\p -> createTriangle Line { p1 = p, p2 = p } p) generatedPoints
  -- in [Triangle { a = Point { x = 200, y = 200 }, b = Point { x = 200, y = 300 }, c = Point { x = 300, y = 250 }}]
  in triangulationTest

getVoronoi :: Geometry -> Voronoi
getVoronoi geometry = [[Point { x = 500, y = 500 }, Point { x = 500, y = 600 }, Point { x = 600, y = 550 }]]