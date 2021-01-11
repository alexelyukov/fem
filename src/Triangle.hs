module Triangle (
  getAllBadTriangles,
  triangles2Nodes,



  fromTriangles,
  fromTriangle,
  whereIsPoint,
  isNeighbors,
  searchAllNeighbors,
  separateNodes,
  -- rebuildNeighbors,
  Triangle(..),
  PointPosition(..),
) where

import Node
import Graphics.Rasterific
import Data.List (nub)

newtype Triangle = Triangle (Node, Node, Node)
data PointPosition = Out | In | OnEdge | OnNode deriving (Eq, Ord, Show)

instance Show Triangle where
   show (Triangle (node1, node2, node3)) =
     show (node1, node2, node3)

instance Eq Triangle where
  Triangle (nodeA1, nodeA2, nodeA3) == Triangle (nodeB1, nodeB2, nodeB3) = nodeA1 == nodeB1 && nodeA2 == nodeB2 && nodeA3 == nodeB3

getAllBadTriangles :: Node -> [Triangle] -> [Triangle]
getAllBadTriangles n = filter (isIntoAroundCircle n)

isIntoAroundCircle :: Node -> Triangle -> Bool
isIntoAroundCircle (Node x y) (Triangle (Node ax ay, Node bx by, Node cx cy)) =
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

triangles2Nodes :: [Triangle] -> [Node]
triangles2Nodes = foldl (\acc (Triangle (node1, node2, node3)) -> node1 : node2 : node3 : acc) []






-- grahamPredicat :: Node -> Node -> Node -> Ordering
-- grahamPredicat (Node x0 y0) (Node x1 y1) (Node x2 y2) =
--   let p1 = (x1 - x0, y1 - y0)
--       p2 = (x2 - x0, y2 - y0)
--       calcfi = \(xp1, yp1) -> case () of {
--         _ | xp1 == 0 && yp1 < 0 -> (-pi) / 2
--           | xp1 == 0 && yp1 > 0 -> pi / 2
--           | otherwise -> atan (yp1 / xp1)
--       }
--   in compare (calcfi p1) (calcfi p2)


whereIsPoint :: Node -> Triangle -> PointPosition
whereIsPoint (Node x0 y0) (Triangle (Node x1 y1, Node x2 y2, Node x3 y3)) =
  let vmz1 = (x2 - x1) * (y0 - y1) - (y2 - y1) * (x0 - x1)
      vmz2 = (x3 - x2) * (y0 - y2) - (y3 - y2) * (x0 - x2)
      vmz3 = (x1 - x3) * (y0 - y3) - (y1 - y3) * (x0 - x3)
  in case () of {
    _ | vmz1 > 0 && vmz2 > 0 && vmz3 > 0 -> In
      | vmz1 < 0 && vmz2 < 0 && vmz3 < 0 -> In
      | vmz1 == 0 && ((vmz2 < 0 && vmz3 < 0) || (vmz2 > 0 && vmz3 > 0)) -> OnEdge
      | vmz2 == 0 && ((vmz1 < 0 && vmz3 < 0) || (vmz1 > 0 && vmz3 > 0)) -> OnEdge
      | vmz3 == 0 && ((vmz1 < 0 && vmz2 < 0) || (vmz1 > 0 && vmz2 > 0)) -> OnEdge
      | (vmz1 == 0 && vmz2 == 0) || (vmz1 == 0 && vmz3 == 0) || (vmz2 == 0 && vmz3 == 0) -> OnNode
      | otherwise -> Out
  }

fromTriangles :: [Triangle] -> [(V2 Float, V2 Float, V2 Float)]
fromTriangles = map fromTriangle

fromTriangle :: Triangle -> (V2 Float, V2 Float, V2 Float)
fromTriangle (Triangle (node1, node2, node3)) = (fromNode node1, fromNode node2, fromNode node3)

searchAllNeighbors :: Triangle -> [Triangle] -> [Triangle]
searchAllNeighbors t = filter (isNeighbors t)

isNeighbors :: Triangle -> Triangle -> Bool
isNeighbors t1 t2 = length (getEdgeNodes t1 t2) == 2

getEdgeNodes :: Triangle -> Triangle -> [Node]
getEdgeNodes (Triangle (nodeA1, nodeA2, nodeA3)) (Triangle (nodeB1, nodeB2, nodeB3))
  = [a | a <- [nodeA1, nodeA2, nodeA3], b <- [nodeB1, nodeB2, nodeB3], a == b]

separateNodes :: Triangle -> Triangle -> ([Node], [Node])
separateNodes t1@(Triangle (nodeA1, nodeA2, nodeA3)) t2@(Triangle (nodeB1, nodeB2, nodeB3)) =
  let edgeNodes = getEdgeNodes t1 t2
      differentNodes = filter (`notElem` edgeNodes) [nodeA1, nodeA2, nodeA3] ++ [nodeB1, nodeB2, nodeB3]
  in (edgeNodes, differentNodes)

-- rebuildNeighbors :: (Triangle, Triangle) -> (Triangle, Triangle)
-- rebuildNeighbors (Triangle (nodeA1, nodeA2, nodeA3), Triangle (nodeB1, nodeB2, nodeB3)) =
--   let ([edgeNode1, edgeNode2], [differentNode1, differentNode2]) = separateNodes (Triangle (nodeA1, nodeA2, nodeA3)) (Triangle (nodeB1, nodeB2, nodeB3))
--   in (Triangle (differentNode1, differentNode2, edgeNode1), Triangle (differentNode1, differentNode2, edgeNode2))
