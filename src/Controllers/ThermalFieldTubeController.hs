module Controllers.ThermalFieldTubeController (
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
url = "thermal-field-tube-triangulation"

getTriangulation :: Geometry -> Triangulation
getTriangulation geometry =
  let generator = mkStdGen 10
      border = getAroundRectangle geometry
      borderPoints = geometry2points geometry
      (generatedPoints, generator') = generatePoints generator geometry 3000
      initialTriangulation = getInitialTriangulation geometry
      Triangle { a = a0, b = b0, c = c0 } = head initialTriangulation
      triangulation = run initialTriangulation (generatedPoints ++ borderPoints)
      (cleanTriangulation, generator'') = foldl (\(acc, generator) t@Triangle { a = a, b = b, c = c } ->
          let cond1 = (a `notElem` [a0, b0, c0]) && (b `notElem` [a0, b0, c0]) && (c `notElem` [a0, b0, c0])
              (condA, generator1) = inGeometry (generateBeamPoint border) generator a geometry
              (condB, generator2) = inGeometry (generateBeamPoint border) generator1 b geometry
              (condC, generator3) = inGeometry (generateBeamPoint border) generator2 c geometry
              Point {x = x1, y = y1 } = a
              Point {x = x2, y = y2 } = b
              Point {x = x3, y = y3 } = c
              mediana = Point { x = (x1 + x2 + x3) / 3, y = (y1 + y2 + y3) / 3}
              (cond2, generator4) = inGeometry (generateBeamPoint border) generator3 mediana geometry
          in case () of {
            _ | not cond1 -> (acc, generator)
              | not condA -> (acc, generator1)
              | not condB -> (acc, generator2)
              | not condC -> (acc, generator3)
              | not cond2 -> (acc, generator4)
              | otherwise -> (t:acc, generator4)
          }
        ) ([], generator') triangulation
      -- success = check cleanTriangulation
  in cleanTriangulation

getVoronoi :: Geometry -> Voronoi
getVoronoi geometry = [[Point { x = 500, y = 500 }, Point { x = 500, y = 600 }, Point { x = 600, y = 550 }]]