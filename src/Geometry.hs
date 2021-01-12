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

getGeometry :: Geometry
getGeometry =
  let nodes1 = [Node (spreadPoints 500 1500 i 50) 500 | i <- [0..50]]
        ++ [Node 1500 (spreadPoints 500 1500 i 50) | i <- [1..49]]
        ++ [Node (spreadPoints 1500 500 i 50) 1500 | i <- [0..50]]
        ++ [Node 500 (spreadPoints 1500 500 i 50) | i <- [1..49]]
      nodes2 = [Node (spreadPoints 700 900 i 10) 700 | i <- [0..10]]
        ++ [Node 900 (spreadPoints 700 1300 i 30) | i <- [1..29]]
        ++ [Node (spreadPoints 900 700 i 10) 1300 | i <- [0..10]]
        ++ [Node 700 (spreadPoints 1300 700 i 30) | i <- [1..29]]
      nodes3 = [Node (spreadPoints 1100 1300 i 10) 700 | i <- [0..10]]
        ++ [Node 1300 (spreadPoints 700 1300 i 30) | i <- [1..29]]
        ++ [Node (spreadPoints 1300 1100 i 10) 1300 | i <- [0..10]]
        ++ [Node 1100 (spreadPoints 1300 700 i 30) | i <- [1..29]]
  in [Figure nodes1 True, Figure nodes2 False, Figure nodes3 False]

getSuperTriangle :: Triangle
getSuperTriangle = Triangle (Node 1000 (-500), Node (-500) 1700, Node 2500 1700)

getBorders :: (Node, Node)
getBorders = (Node 500 500, Node 1500 1500)

geometry2nodes :: Geometry -> [Node]
geometry2nodes [] = []
geometry2nodes ((Figure nodes _):fs) = nodes ++ geometry2nodes fs

drawGeometry :: Geometry -> Drawing PixelRGBA8 ()
drawGeometry [figure1, figure2, figure3] = do
  drawFigure figure1 5 (PixelRGBA8 0x00 0x00 0x00 255) (PixelRGBA8 0xDD 0xDD 0xDD 255) Solid
  drawFigure figure2 5 (PixelRGBA8 0x00 0x00 0x00 255) (PixelRGBA8 0xFF 0xFF 0xFF 255) Solid
  drawFigure figure3 5 (PixelRGBA8 0x00 0x00 0x00 255) (PixelRGBA8 0xFF 0xFF 0xFF 255) Solid

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
