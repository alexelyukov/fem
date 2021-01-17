module Generator (
  generatePoints,
) where

import System.Random
import Types
import Data.List
import Debug.Trace

generatePoints :: StdGen -> Geometry -> Integer -> ([Point], StdGen)
generatePoints generator geometry count =
  let border = getAroundRectangle geometry
  in foldr (
    \value acc -> case generatePoint acc border geometry of
      (Just value, generator') -> (value : fst acc, generator')
      (Nothing, generator') -> (fst acc, generator')
  ) ([], generator) [1..count]

generatePoint :: ([Point], StdGen) -> Rectangle -> Geometry -> (Maybe Point, StdGen)
generatePoint (points, generator) border geometry =
  let (x0, generator') = random generator
      (y0, generator'') = random generator'
      Rectangle { leftTop = Point { x = minX, y = minY }, rightBottom = Point { x = maxX, y = maxY } } = border
      point = Point { x = minX + x0 * (maxX - minX), y = minY + y0 * (maxY - minY) }
      (isInGeometry, generator''') = inGeometry (generateBeamPoint border) generator'' point geometry
  in  if isInGeometry && isFarFromAnyPoint point points geometry
      then (Just point, generator''')
      else (Nothing, generator''')

getAroundRectangle :: Geometry -> Rectangle
getAroundRectangle geometry =
  let points = geometry2points geometry
      Point { x = minX } = minimumBy (\Point { x = x1 } Point { x = x2 } -> compare x1 x2) points
      Point { y = minY } = minimumBy (\Point { y = y1 } Point { y = y2 } -> compare y1 y2) points
      Point { x = maxX } = maximumBy (\Point { x = x1 } Point { x = x2 } -> compare x1 x2) points
      Point { y = maxY } = maximumBy (\Point { y = y1 } Point { y = y2 } -> compare y1 y2) points
  in Rectangle { leftTop = Point { x = minX, y = minY }, rightBottom = Point { x = maxX, y = maxY } }

generateBeamPoint :: Rectangle -> StdGen -> (Point, StdGen)
generateBeamPoint Rectangle { leftTop = p1@Point { x = minX, y = minY }, rightBottom = p2@Point { x = maxX, y = maxY } } generator = 
  let (x0, y0) = ((minX + maxX) / 2, (minY + maxY) / 2)
      r = 0.5 * distance p1 p2
      (rnd, generator') = random generator
      fi = 2 * pi * rnd
  in (Point { x = x0 + r * cos (2 * pi * rnd), y = y0 + r * sin (2 * pi * rnd)}, generator')