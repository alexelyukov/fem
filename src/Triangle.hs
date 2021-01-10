module Triangle (
  whereIsPoint,
  Triangle(..),
  PointPosition(..),
) where

import Node

data Triangle = Triangle (Node, Node, Node) (Maybe Triangle, Maybe Triangle, Maybe Triangle)
data PointPosition = Out | In | OnEdge | OnNode deriving (Eq, Ord, Show)

whereIsPoint :: Triangle -> Node -> PointPosition
whereIsPoint (Triangle (Node x1 y1, Node x2 y2, Node x3 y3) _) (Node x0 y0) =
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
