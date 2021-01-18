module Geometry (
    geometry2points,
    triangles2points,
    points2triangles,
    inGeometry,
    isFarFromAnyPoint,
    distance,
    createTriangle,
    getInitialTriangulation,
    getAroundRectangle,
    getAroundCircle,
    getBadTriangles,
    polarAngleSort,
) where

import Types
import System.Random
import Data.List

distance :: Point -> Point -> Float
distance Point { x = x1, y = y1 } Point { x = x2, y = y2 } = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- преобразования
geometry2points :: Geometry -> [Point]
geometry2points = foldl (\acc figure -> figure2points figure ++ acc) []

triangles2points :: [Triangle] -> [Point]
triangles2points = foldl (\acc Triangle { a = a, b = b, c = c } -> a : b : c : acc) []

points2triangles :: Point -> [Point] -> [Triangle]
points2triangles point [_] = []
points2triangles a (b:c:ps) = Triangle { a = a, b = b, c = c } : points2triangles a (c:ps)

figure2points :: Figure -> [Point]
figure2points Figure { points = points } = points

polygon2lines :: Polygon -> [Line]
polygon2lines polygon@(p1:_) = polygon2lines' $ polygon ++ [p1]

polygon2lines' :: Polygon -> [Line]
polygon2lines' [_] = []
polygon2lines' (p1:p2:pn) = Line { p1 = p1, p2 = p2 } : polygon2lines' (p2:pn)

-- проверка сгенерированной точки
isFarFromAnyPoint :: Point -> [Point] -> Geometry -> Bool
isFarFromAnyPoint point points geometry =
    null [True | p <- geometry2points geometry ++ points, distance p point < 10]

inGeometry :: (StdGen -> (Point, StdGen)) -> StdGen -> Point -> Geometry -> (Bool, StdGen)
inGeometry fn generator point = foldl (
        \(accOk, _) figure ->
            let (ok, generator') = inFigure fn generator point figure
            in (accOk && ok, generator')
    ) (True, generator)

inFigure :: (StdGen -> (Point, StdGen)) -> StdGen -> Point -> Figure -> (Bool, StdGen)
inFigure fn generator point Figure { points = polygon, io = io } =
    let (isInPolygon, generator') = inPolygon fn generator point polygon
    in if io then (isInPolygon, generator') else (not isInPolygon, generator')

inPolygon :: (StdGen -> (Point, StdGen)) -> StdGen -> Point -> Polygon -> (Bool, StdGen)
inPolygon fn generator point polygon =
    let (generatedPoint, generator') = fn generator
        line = Line { p1 = generatedPoint, p2 = point }
        intersection = polygonIntersect line polygon
    in  case intersection of {
        (_, False) -> inPolygon fn generator' point polygon;
        (intersectionCount, _) -> (odd intersectionCount, generator')
    }

polygonIntersect :: Line -> Polygon -> (Int, Bool)
polygonIntersect line polygon =
    let combinations = map (linesIntersect line) (polygon2lines polygon)
        isGoodPoint = not (any (`elem` [TheSame, OnEdge]) combinations)
        intersectionCount = length (filter (== Intersect) combinations)
    in (intersectionCount, isGoodPoint)

linesIntersect :: Line -> Line -> LineCombination
linesIntersect
    Line { p1 = Point { x = x1, y = y1 }, p2 = Point { x = x2, y = y2 } }
    Line { p1 = Point { x = x3, y = y3 }, p2 = Point { x = x4, y = y4 } } =
        let d = (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)
            a = (x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)
            b = (x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)
        in case () of {
            _ | d == 0 && (a == 0 || b == 0) -> TheSame
              | d == 0 -> Parallel
              | a / d > 0 && a / d < 1 && b / d > 0 && b / d < 1 -> Intersect
              | a / d == 0 || a / d == 1 || b / d == 0 || b / d == 1 -> OnEdge
              | otherwise -> NotIntersect
        }

getAroundRectangle :: Geometry -> Rectangle
getAroundRectangle geometry =
  let points = geometry2points geometry
      Point { x = minX } = minimumBy (\Point { x = x1 } Point { x = x2 } -> compare x1 x2) points
      Point { y = minY } = minimumBy (\Point { y = y1 } Point { y = y2 } -> compare y1 y2) points
      Point { x = maxX } = maximumBy (\Point { x = x1 } Point { x = x2 } -> compare x1 x2) points
      Point { y = maxY } = maximumBy (\Point { y = y1 } Point { y = y2 } -> compare y1 y2) points
  in Rectangle { leftTop = Point { x = minX, y = minY }, rightBottom = Point { x = maxX, y = maxY } }

getAroundCircle :: Rectangle -> Circle
getAroundCircle Rectangle { leftTop = p1@Point { x = minX, y = minY }, rightBottom = p2@Point { x = maxX, y = maxY } } = 
    let (x0, y0) = ((minX + maxX) / 2, (minY + maxY) / 2)
        r0 = 0.5 * distance p1 p2
    in Circle { p = Point { x = x0, y = y0 }, r = r0 }

getInitialTriangulation :: Geometry -> Triangulation
getInitialTriangulation geometry =
    let Circle { p = Point { x = x0, y = y0 }, r = r0 } = getAroundCircle $ getAroundRectangle geometry
    in [Triangle {
        a = Point { x = x0, y = y0 - 2 * r0 },
        b = Point { x = x0 + r0 * sqrt 3, y = y0 + r0 },
        c = Point { x = x0 - r0 * sqrt 3, y = y0 + r0 }
    }]

getBadTriangles :: Point -> [Triangle] -> [Triangle]
getBadTriangles point = filter (intoAroundCircle point)

intoAroundCircle :: Point -> Triangle -> Bool
intoAroundCircle Point { x = x, y = y } Triangle { a = Point { x = ax, y = ay }, b = Point { x = bx, y = by }, c = Point { x = cx, y = cy } } =
  let d = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
      centerX = ((ax ^ 2 + ay ^ 2) * (by - cy) + (bx ^ 2 + by ^ 2) * (cy - ay) + (cx ^ 2 + cy ^ 2) * (ay - by)) / d
      centerY = ((ax ^ 2 + ay ^ 2) * (cx - bx) + (bx ^ 2 + by ^ 2) * (ax - cx) + (cx ^ 2 + cy ^ 2) * (bx - ax)) / d
      a = sqrt ((bx - ax) ^ 2 + (by - ay) ^ 2)
      b = sqrt ((cx - bx) ^ 2 + (cy - by) ^ 2)
      c = sqrt ((ax - cx) ^ 2 + (ay - cy) ^ 2)
      p = (a + b + c) / 2
      r = a * b * c / (4 * sqrt (p * (p - a) * (p - b) * (p - c)))
      len = sqrt ((centerX - x) ^ 2 + (centerY - y) ^ 2)
  in len < r

polarAngleSort :: Point -> [Point] -> [Point]
polarAngleSort point = sortBy (sortPredicat point)

sortPredicat :: Point -> Point -> Point -> Ordering
sortPredicat a b c =
  let angle1 = getAbsoluteAngle a b
      angle2 = getAbsoluteAngle a c
  in compare angle1 angle2

getAbsoluteAngle :: Point -> Point -> Float
getAbsoluteAngle Point { x = x1, y = y1 } Point { x = x2, y = y2 }
  | x > 0 = atan (y / x)
  | x < 0 = atan (y / x) + pi
  | x == 0 && y > 0 = pi / 2
  | x == 0 && y < 0 = 3 * pi / 2
  where x = x2 - x1
        y = y2 - y1

-- временные функции
createTriangle :: Line -> Point -> Triangle
createTriangle Line { p1 = b, p2 = c } a = Triangle { a = a, b = b, c = c}
