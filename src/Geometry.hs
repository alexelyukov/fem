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
  let nodes1 = [Node (1000 + 500 * cos(2 * pi * i / 200)) (1000 + 500 * sin(2 * pi * i / 200)) | i <- [1..200]]
      nodes2 = [Node (1000 + 0.5*500*cos(pi/2) + 130 * cos(2 * pi * i / 40)) (1000 - 0.5*500*sin(pi/2) + 130 * sin(2 * pi * i / 40)) | i <- [1..40]]
      nodes3 = [Node (1000 - 0.5*500*cos(pi/6) + 130 * cos(2 * pi * i / 40)) (1000 + 0.5*500*sin(pi/6) + 130 * sin(2 * pi * i / 40)) | i <- [1..40]]
      nodes4 = [Node (1000 + 0.5*500*cos(pi/6) + 130 * cos(2 * pi * i / 40)) (1000 + 0.5*500*sin(pi/6) + 130 * sin(2 * pi * i / 40)) | i <- [1..40]]
  in [Figure nodes1 True, Figure nodes2 False, Figure nodes3 False, Figure nodes4 False]

-- !!! callback
getSuperTriangle :: Triangle
getSuperTriangle = Triangle (Node 1000 (-500), Node (-500) 1700, Node 2500 1700)

-- !!! callback
getBorders :: (Node, Node)
getBorders = (Node 500 500, Node 1500 1500)

-- !!! callback
drawGeometry :: Geometry -> Drawing PixelRGBA8 ()
drawGeometry [figure1, figure2, figure3, figure4] = do
  drawFigure figure1 5 (PixelRGBA8 0x00 0x00 0x00 255) (PixelRGBA8 0xDD 0xDD 0xDD 255) Solid
  drawFigure figure2 5 (PixelRGBA8 0x00 0x00 0x00 255) (PixelRGBA8 0xFF 0xFF 0xFF 255) Solid
  drawFigure figure3 5 (PixelRGBA8 0x00 0x00 0x00 255) (PixelRGBA8 0xFF 0xFF 0xFF 255) Solid
  drawFigure figure4 5 (PixelRGBA8 0x00 0x00 0x00 255) (PixelRGBA8 0xFF 0xFF 0xFF 255) Solid

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
