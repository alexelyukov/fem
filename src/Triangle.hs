module Triangle (
  fromTriangles,
  fromTriangle,
  whereIsPoint,
  isNeighbors,
  searchAllNeighbors,
  separateNodes,
  Triangle(..),
  PointPosition(..),
) where

import Node
import Graphics.Rasterific

newtype Triangle = Triangle (Node, Node, Node)
data PointPosition = Out | In | OnEdge | OnNode deriving (Eq, Ord, Show)

instance Show Triangle where
   show (Triangle (node1, node2, node3)) =
     show (node1, node2, node3)

instance Eq Triangle where
  Triangle (nodeA1, nodeA2, nodeA3) == Triangle (nodeB1, nodeB2, nodeB3) = nodeA1 == nodeB1 && nodeA2 == nodeB2 && nodeA3 == nodeB3

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
separateNodes (Triangle (nodeA1, nodeA2, nodeA3)) (Triangle (nodeB1, nodeB2, nodeB3)) =
  let edgeNodes = getEdgeNodes (Triangle (nodeA1, nodeA2, nodeA3)) (Triangle (nodeB1, nodeB2, nodeB3))
      differentNodes1 = filter (`notElem` edgeNodes) [nodeA1, nodeA2, nodeA3]
      differentNodes2 = filter (`notElem` edgeNodes) [nodeB1, nodeB2, nodeB3]
  in (edgeNodes, differentNodes1 ++ differentNodes2)
