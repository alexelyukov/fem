module Node (
  Node(..),
  toNodes,
  toNode,
  fromNodes,
  fromNode,
  distance,
) where

import Graphics.Rasterific

data Node = Node Float Float deriving (Eq, Show)

toNodes :: [V2 Float] -> [Node]
toNodes = map toNode

toNode :: V2 Float -> Node
toNode (V2 x y) = Node x y

fromNodes :: [Node] -> [V2 Float]
fromNodes = map fromNode

fromNode :: Node -> V2 Float
fromNode (Node x y) = V2 x y

distance :: Node -> Node -> Float
distance (Node x1 y1) (Node x2 y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)