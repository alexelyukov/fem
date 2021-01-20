module Controllers.ElectromagneticFieldMagnetController (
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
url = "electromagnetic-field-magnet-triangulation"

getTriangulation :: Geometry -> Triangulation
getTriangulation geometry =
  let generator = mkStdGen 10
      border = getAroundRectangle geometry
      borderPoints = geometry2points geometry
      (generatedPoints, generator') = generatePoints generator geometry 10000
      initialTriangulation = getInitialTriangulation geometry
      Triangle { a = a0, b = b0, c = c0 } = head initialTriangulation
      triangulation = run initialTriangulation (generatedPoints ++ borderPoints)
      (triangulation', generator'') = cleanTriangulation triangulation (head initialTriangulation) border geometry generator'
      -- success = check triangulation'
  in triangulation'

getVoronoi :: Geometry -> Voronoi
getVoronoi geometry = [[Point { x = 500, y = 500 }, Point { x = 500, y = 600 }, Point { x = 600, y = 550 }]]