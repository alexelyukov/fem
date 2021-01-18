module Delaunay (
  check,
  run,
) where

import Types
import Data.List
import Geometry
import Debug.Trace

run :: Triangulation -> [Point] -> Triangulation
run = foldl addPoint

check :: Triangulation -> Bool
check triangulation =
  let points = triangles2points triangulation
  in foldl (\acc triangle@Triangle { a = a, b = b, c = c } ->
    let points' = filter (\point -> point `notElem` [a, b, c]) points
    in acc && checkPoints triangle points'
  ) True triangulation

-- private

checkPoints :: Triangle -> [Point] -> Bool
checkPoints triangle = foldl (\acc point -> acc && checkPoint triangle point) True

checkPoint :: Triangle -> Point -> Bool
checkPoint Triangle { a = Point { x = x1, y = y1 }, b = Point { x = x2, y = y2 }, c = Point { x = x3, y = y3 } } Point { x = x0, y = y0 } =
  let sa = (x0 - x1) * (x0 - x3) + (y0 - y1) * (y0 - y3)
      sb = (x2 - x1) * (x2 - x3) + (y2 - y1) * (y2 - y3)
      k1 = (x0 - x1) * (y0 - y3) - (x0 - x3) * (y0 - y1)
      k2 = (x2 - x1) * (y2 - y3) - (x2 - x3) * (y2 - y1)
  in case () of {
    _ | sa < 0 && sb < 0 -> False
      | sa >= 0 && sb >= 0 -> True
      | otherwise -> k1 * sb + k2 * sa >= 0
  }

addPoint :: Triangulation -> Point -> Triangulation
addPoint triangulation point =
  let (badTriangles, goodPoint) = getBadTriangles' point triangulation
      badNodes = nub $ triangles2points badTriangles
      cleanTriangles = filter (`notElem` badTriangles) triangulation
      poligon@(h:_) = polarAngleSort point badNodes
      newTriangles = points2triangles point (poligon ++ [h])
  in if goodPoint
     then newTriangles ++ cleanTriangles
     else triangulation
