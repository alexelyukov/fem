module Delaunay (
  check,
  run,
) where

import Triangle
import Node
import Data.List

check :: Node -> Triangle -> Bool
check (Node x0 y0) (Triangle (Node x1 y1, Node x2 y2, Node x3 y3)) =
  let sa = (x0 - x1) * (x0 - x3) + (y0 - y1) * (y0 - y3)
      sb = (x2 - x1) * (x2 - x3) + (y2 - y1) * (y2 - y3)
      k1 = (x0 - x1) * (y0 - y3) - (x0 - x3) * (y0 - y1)
      k2 = (x2 - x1) * (y2 - y3) - (x2 - x3) * (y2 - y1)
  in case () of {
    _ | sa < 0 && sb < 0 -> False
      | sa >= 0 && sb >= 0 -> True
      | otherwise -> k1 * sb + k2 * sa >= 0
  }

run :: [Triangle] -> [Node] -> [Triangle]
run = foldl addNode

-- private

addNode :: [Triangle] -> Node -> [Triangle]
addNode ts node =
  let badTriangles = getBadTriangles node ts
      badNodes = nub $ triangles2Nodes badTriangles
      cleanTriangles = filter (`notElem` badTriangles) ts
      poligon@(h:_) = polarAngleSort node badNodes
      newTriangles = nodes2Triangles node (poligon ++ [h])
  in newTriangles ++ cleanTriangles
