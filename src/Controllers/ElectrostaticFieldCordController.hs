module Controllers.ElectrostaticFieldCordController (
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
url = "electrostatic-field-cord-triangulation"

getTriangulation :: Geometry -> Triangulation
getTriangulation geometry =
  let generator = mkStdGen 10
      (generatedPoints, generator') = generatePoints generator geometry 3000
      borderPoints = geometry2points geometry
      initialTriangulation = getInitialTriangulation geometry
      Triangle { a = a0, b = b0, c = c0 } = head initialTriangulation
      triangulation = run initialTriangulation (generatedPoints ++ borderPoints)
      cleanTriangulation = filter (\Triangle { a = a, b = b, c = c } ->
                                      (a `notElem` [a0, b0, c0])
                                    && (b `notElem` [a0, b0, c0])
                                    && (c `notElem` [a0, b0, c0])
                                  ) triangulation
      -- success = check cleanTriangulation
  in cleanTriangulation

getVoronoi :: Geometry -> Voronoi
getVoronoi geometry = [[Point { x = 500, y = 500 }, Point { x = 500, y = 600 }, Point { x = 600, y = 550 }]]