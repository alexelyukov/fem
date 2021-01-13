module Geometry (
  Figure(..),
  Geometry(..),

  getGeometry,
  getSuperTriangle,
  getBorders,
  drawGeometry,
  geometry2nodes,
  isInGeometry,
  isFarFromAnyNode,
) where

import Node
import Graphics.Rasterific ( Drawing )
import Codec.Picture ( PixelRGBA8(..) )
import Drawer
import Triangle

data Figure = Figure [Node] Bool
type Geometry = [Figure]

-- !!! callback
getGeometry :: Geometry
getGeometry =
  let n1 = 8
      n2 = 5
      nodes1 = [Node (1000 + 500 * cos(2 * pi * i / n1)) (1000 + 500 * sin(2 * pi * i / n1)) | i <- [1..n1-4]]
      -- nodes2 = [Node (1000 + 300 * cos(2 * pi * i / n2)) (1000 + 300 * sin(2 * pi * i / n2)) | i <- [1..n2]]
  in [Figure nodes1 True]

-- !!! callback
getSuperTriangle :: Triangle
getSuperTriangle = Triangle (Node 1000 (-500), Node (-500) 1700, Node 2500 1700)

-- !!! callback
getBorders :: (Node, Node)
getBorders = (Node 500 500, Node 1500 1500)

-- !!! callback
drawGeometry :: Geometry -> Drawing PixelRGBA8 ()
drawGeometry [figure1] = do
  drawFigure figure1 5 (PixelRGBA8 0x00 0x00 0x00 255) (PixelRGBA8 0xDD 0xDD 0xDD 255) Solid

geometry2nodes :: Geometry -> [Node]
geometry2nodes [] = []
geometry2nodes ((Figure nodes _):fs) = nodes ++ geometry2nodes fs

isInGeometry :: Node -> Geometry -> Bool
isInGeometry n = foldl (\acc value -> acc && inFigure n value) True

isFarFromAnyNode :: Node -> [Node] -> Geometry -> Bool
isFarFromAnyNode node ns geometry =
  null ([True | p <- ns ++ geometry2nodes geometry, distance p node < 20])

-- private

inFigure :: Node -> Figure -> Bool
inFigure n0 (Figure n@(n1:_) inout) =
  let poligon = n ++ [n1]
      isIn = isInside n0 poligon
  in if inout then isIn else not isIn

isInside :: Node -> [Node] -> Bool
isInside _ [_] = True
isInside n (n1:n2:ns) = (leftOrRight n n1 n2 == GT) && isInside n (n2:ns)

spreadPoints :: Float -> Float -> Float -> Float -> Float
spreadPoints x1 x2 i n = x1 + (x2 - x1) * (i / n)

drawFigure :: Figure -> Size -> PixelRGBA8 -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawFigure (Figure n _) size colorBorder color linetype = do
  let points = fromNodes n
  drawPolygon points size colorBorder linetype
  fillPolygon points color