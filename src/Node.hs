module Node (
  Node(..),
  toNodes,
  toNode,
  fromNodes,
  fromNode,
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