{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (
    Point(..),
    Line(..),
    Rectangle(..),
    Circle(..),
    Triangle(..),
    Figure(..),
    TriangulationRequest(..),
    TriangulationResponse(..),
    Polygon(..),
    Geometry(..),
    Triangulation(..),
    Voronoi(..),
    geometry2points,
    inGeometry,
    isFarFromAnyPoint,
    distance,
    createTriangle,
) where

import GHC.Generics
import Data.Aeson
import System.Random
import Debug.Trace

data Point = Point { x :: Float, y :: Float } deriving (Eq, Generic, Show)
instance FromJSON Point
instance ToJSON Point

data Line = Line { p1 :: Point, p2 :: Point } deriving (Eq, Generic, Show)
instance FromJSON Line
instance ToJSON Line

data Rectangle = Rectangle { leftTop :: Point, rightBottom :: Point } deriving (Generic, Show)
instance FromJSON Rectangle
instance ToJSON Rectangle

data Circle = Circle { p :: Point, r :: Float } deriving (Generic, Show)
instance FromJSON Circle
instance ToJSON Circle

data Triangle = Triangle { a :: Point, b :: Point, c :: Point } deriving (Generic, Show)
instance FromJSON Triangle
instance ToJSON Triangle

data Figure = Figure { points :: [Point], io :: Bool } deriving (Generic, Show)
instance FromJSON Figure
instance ToJSON Figure

newtype TriangulationRequest = TriangulationRequest { geometry :: Geometry } deriving (Generic, Show)
instance FromJSON TriangulationRequest
instance ToJSON TriangulationRequest

data TriangulationResponse = TriangulationResponse { triangulation :: Triangulation, voronoi :: Voronoi } deriving (Generic, Show)
instance FromJSON TriangulationResponse
instance ToJSON TriangulationResponse

type Polygon = [Point]
type Geometry = [Figure]
type Triangulation = [Triangle]
type Voronoi = [Polygon]

data LineCombination = TheSame | Parallel | Intersect | NotIntersect | OnEdge deriving (Eq, Show)

distance :: Point -> Point -> Float
distance Point { x = x1, y = y1 } Point { x = x2, y = y2 } = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- преобразования
geometry2points :: Geometry -> [Point]
geometry2points = foldl (\acc figure -> figure2points figure ++ acc) []

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

-- временные функции
createTriangle :: Line -> Point -> Triangle
createTriangle Line { p1 = b, p2 = c } a = Triangle { a = a, b = b, c = c}
