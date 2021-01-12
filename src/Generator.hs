module Generator (
  generatePoints,
) where

import System.Random
import Geometry
import Node

generatePoints :: StdGen -> Geometry -> [Node] -> [Node]
generatePoints gen0 geometry acc0 =
  let (node, gen1) = generatePoint gen0
      acc1 = node:acc0
  in if isInGeometry node geometry && isFarFromAnyNode node acc0 geometry
    then node : generatePoints gen1 geometry acc1
    else generatePoints gen1 geometry acc0

-- private

generatePoint :: StdGen -> (Node, StdGen)
generatePoint gen =
  let (x, genX) = random gen
      (y, genY) = random genX
  in (packToBorders (x,y), genY)

packToBorders :: (Float, Float) -> Node
packToBorders (x, y) = 
  let (Node x0 y0, Node x1 y1) = getBorders
  in Node (x0 + x * (x1 - x0)) (y0 + y * (y1 - y0))
