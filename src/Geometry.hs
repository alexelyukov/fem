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
  let n = 10
      alpha = 2*pi/n
      beta = alpha*0.7
      gamma = alpha-beta
      r1 = 500
      r2 = 100
      innerRound = [Node (1000+r2*cos(gamma*i/20)) (1000+r2*sin(gamma*i/20)) | i <- [1..19]]
      outerRound = [Node (1000+r1*cos(beta*i/20)) (1000+r1*sin(beta*i/20)) | i <- [1..19]]
      border1 = [Node (1000+r2+(r1-r2)*i/20) 1000 | i <- [0..20]]
      border2 = reverse border1

      step = map (rotate (-beta)) border1
        ++ map (rotate (-beta)) outerRound
        ++ border2
        ++ innerRound

      nodes1 = foldl (\acc value -> map (rotate value) step ++ acc) step [alpha*i | i <- [1..n-1]]
  in [Figure step True]

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

rotate :: Float -> Node -> Node
rotate fi (Node x y) =
  let sinA = sin fi
      cosA = cos fi
  in Node (x*cosA-y*sinA) (x*sinA+y*cosA)

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
