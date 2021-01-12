module Node (
  Node(..),
  fromNodes,
  fromNode,
  distance,
  polarAngleSort,
  leftOrRight,
) where

import Graphics.Rasterific
import Data.List

data Node = Node Float Float deriving (Eq, Show)

fromNodes :: [Node] -> [V2 Float]
fromNodes = map fromNode

fromNode :: Node -> V2 Float
fromNode (Node x y) = V2 x y

distance :: Node -> Node -> Float
distance (Node x1 y1) (Node x2 y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

polarAngleSort :: Node -> [Node] -> [Node]
polarAngleSort x0 = sortBy (sortPredicat x0)

leftOrRight :: Node -> Node -> Node -> Ordering
leftOrRight (Node c0 c1) (Node a0 a1) (Node b0 b1) =
  compare ((b0 - a0) * (c1 - b1) - (b1 - a1) * (c0 - b0)) 0

-- private

sortPredicat :: Node -> Node -> Node -> Ordering
sortPredicat p0 p1 p2 =
  let angle1 = getAbsoluteAngle p0 p1
      angle2 = getAbsoluteAngle p0 p2
  in compare angle1 angle2

getAbsoluteAngle :: Node -> Node -> Float
getAbsoluteAngle (Node x1 y1) (Node x2 y2)
  | x > 0 = atan (y / x)
  | x < 0 = atan (y / x) + pi
  | x == 0 && y > 0 = pi / 2
  | x == 0 && y < 0 = 3 * pi / 2
  where x = x2 - x1
        y = y2 - y1