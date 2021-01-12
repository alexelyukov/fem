module Graham (
  leftOrRight,
  splitNodes,
  step1,
  step2,
  step3,
  grahamScan,
) where

import Node
import Data.List

leftOrRight :: Node -> Node -> Node -> Ordering
leftOrRight (Node c0 c1) (Node a0 a1) (Node b0 b1) =
  compare ((b0 - a0) * (c1 - b1) - (b1 - a1) * (c0 - b0)) 0

splitNodes :: Node -> ([Node], [Node]) -> ([Node], [Node])
splitNodes _ res@(s, [_]) = res
splitNodes minXNode (siblings, baseNode:node:nodes) =
  case leftOrRight minXNode baseNode node of {
    EQ -> splitNodes minXNode (baseNode:siblings, node:nodes);
    _  -> case siblings of {
      [] -> ([], baseNode:node:nodes);
      _  -> (sortBy (\n1 n2 -> compare (distance n1 minXNode) (distance n2 minXNode)) (baseNode:siblings), node:nodes)
    }
  }

step1 :: [Node] -> Node
step1 = minimumBy (\(Node x1 _) (Node x2 _) -> compare x1 x2)

step2 :: Node -> [Node] -> [Node]
step2 n = sortBy (`leftOrRight` n)

step3 :: [Node] -> [Node] -> [Node]
step3 s [] = s
step3 s@(st1:st2:sts) n@(node:nodes) =
  case leftOrRight st2 st1 node of {
    GT -> step3 (node:s) nodes;
    LT -> step3 (st2:sts) n;
    EQ -> step3 (node:s) nodes
  }

grahamScan :: [Node] -> [Node]
grahamScan n =
  let minXNode = step1 n
      nodesForSorting = filter ( /= minXNode) n
      sortedNodes0 = step2 minXNode nodesForSorting
      (beginSiblings, sortedNodes1) = splitNodes minXNode ([], sortedNodes0)
      (finishSiblings, sortedNodes2) = splitNodes minXNode ([], reverse sortedNodes1)
      (h:t) = beginSiblings ++ reverse sortedNodes2 ++ reverse finishSiblings
  in reverse $ step3 [h, minXNode] t
