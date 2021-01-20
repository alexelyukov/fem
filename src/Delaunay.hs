module Delaunay (
  check,
  run,
  cleanTriangulation,
) where

import Types
import Data.List
import Geometry

import System.Random
import Debug.Trace
import Generator

run :: Triangulation -> [Point] -> Triangulation
run = foldl addPoint

check :: Triangulation -> Bool
check triangulation =
  let points = triangles2points triangulation
  in foldl (\acc triangle@Triangle { a = a, b = b, c = c } ->
    let points' = filter (\point -> point `notElem` [a, b, c]) points
    in acc && checkPoints triangle points'
  ) True triangulation

cleanTriangulation :: Triangulation -> Triangle -> Rectangle -> Geometry -> StdGen -> (Triangulation, StdGen)
cleanTriangulation
  triangulation
  Triangle { a = a0, b = b0, c = c0 }
  border
  geometry
  generator' = foldl (\(acc, generator) t@Triangle { a = a, b = b, c = c } ->
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

-- private

checkPoints :: Triangle -> [Point] -> Bool
checkPoints triangle = foldl (\acc point -> acc && not (intoAroundCircle point triangle)) True

addPoint :: Triangulation -> Point -> Triangulation
addPoint triangulation point =
  let badTriangles = getBadTriangles point triangulation
      badNodes = nub $ triangles2points badTriangles
      cleanTriangles = filter (`notElem` badTriangles) triangulation
      poligon@(h:_) = polarAngleSort point badNodes
      newTriangles = points2triangles point (poligon ++ [h])
  in newTriangles ++ cleanTriangles
