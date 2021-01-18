module Generator (
  generatePoints,
) where

import System.Random
import Types
import Data.List
import Debug.Trace
import Geometry

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

generateBeamPoint :: Rectangle -> StdGen -> (Point, StdGen)
generateBeamPoint rectangle generator = 
  let Circle { p = Point { x = x0, y = y0 }, r = r0 } = getAroundCircle rectangle
      (rnd, generator') = random generator
      fi = 2 * pi * rnd
  in (Point { x = x0 + r0 * cos (2 * pi * rnd), y = y0 + r0 * sin (2 * pi * rnd)}, generator')
