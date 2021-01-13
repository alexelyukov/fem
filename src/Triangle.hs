module Triangle (
  Triangle(..),
  PointPosition(..),

  getBadTriangles,
  triangles2Nodes,
  nodes2Triangles,
  fromTriangles,
  getMedian,




  isIntoAroundCircle,
) where

import Node
import Graphics.Rasterific

newtype Triangle = Triangle (Node, Node, Node)
data PointPosition = Out | In | OnEdge | OnNode deriving (Eq, Ord, Show)

instance Show Triangle where
  show (Triangle (node1, node2, node3)) = show (node1, node2, node3)

instance Eq Triangle where
  Triangle (nodeA1, nodeA2, nodeA3) == Triangle (nodeB1, nodeB2, nodeB3) = nodeA1 == nodeB1 && nodeA2 == nodeB2 && nodeA3 == nodeB3

getBadTriangles :: Node -> [Triangle] -> [Triangle]
getBadTriangles n = filter (isIntoAroundCircle n)

triangles2Nodes :: [Triangle] -> [Node]
triangles2Nodes = foldl (\acc (Triangle (node1, node2, node3)) -> node1 : node2 : node3 : acc) []

nodes2Triangles :: Node -> [Node] -> [Triangle]
nodes2Triangles n [_] = []
nodes2Triangles n (p1:p2:ps) = Triangle (n, p1, p2) : nodes2Triangles n (p2:ps)

fromTriangles :: [Triangle] -> [(V2 Float, V2 Float, V2 Float)]
fromTriangles = map fromTriangle

getMedian :: Triangle -> Node
getMedian (Triangle (Node x1 y1, Node x2 y2, Node x3 y3)) = Node ((x1 + x2 + x3) / 3) ((y1 + y2 + y3) / 3)

-- private

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
  in len <= r

fromTriangle :: Triangle -> (V2 Float, V2 Float, V2 Float)
fromTriangle (Triangle (node1, node2, node3)) = (fromNode node1, fromNode node2, fromNode node3)
