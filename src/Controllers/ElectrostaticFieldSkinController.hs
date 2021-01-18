module Controllers.ElectrostaticFieldSkinController (
  handler,
  url
) where

import Types
import Data.Aeson
import System.Random
import Generator
import Geometry
import Delaunay

handler :: Geometry -> String
handler geometry =
  let triangulation = getTriangulation geometry
      voronoi = getVoronoi geometry
  in show $ encode (TriangulationResponse {triangulation = triangulation, voronoi = voronoi})

url :: String
url = "electrostatic-field-skin-triangulation"

getTriangulation :: Geometry -> Triangulation
getTriangulation geometry =
  let generator = mkStdGen 10
      (generatedPoints, generator') = generatePoints generator geometry 300
      borderPoints = geometry2points geometry
      -- triangulationTest = map (\p -> createTriangle Line { p1 = p, p2 = p } p) generatedPoints
      triangulation = run (getInitialTriangulation geometry) borderPoints
  -- in [Triangle { a = Point { x = 200, y = 200 }, b = Point { x = 200, y = 300 }, c = Point { x = 300, y = 250 }}]
  in triangulation

getVoronoi :: Geometry -> Voronoi
getVoronoi geometry = [[Point { x = 500, y = 500 }, Point { x = 500, y = 600 }, Point { x = 600, y = 550 }]]