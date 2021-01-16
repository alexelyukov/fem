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
) where

import GHC.Generics
import Data.Aeson

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
