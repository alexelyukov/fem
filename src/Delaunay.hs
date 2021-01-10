module Delaunay (
  check,
) where

import Graphics.Rasterific

check :: (V2 Float, V2 Float, V2 Float) -> V2 Float -> Bool
check (V2 x1 y1, V2 x2 y2, V2 x3 y3) (V2 x0 y0) =
  let sa = (x0 - x1) * (x0 - x3) + (y0 - y1) * (y0 - y3)
      sb = (x2 - x1) * (x2 - x3) + (y2 - y1) * (y2 - y3)
      k1 = (x0 - x1) * (y0 - y3) - (x0 - x3) * (y0 - y1)
      k2 = (x2 - x1) * (y2 - y3) - (x2 - x3) * (y2 - y1)
  in case () of {
    _ | sa < 0 && sb < 0 -> False
      | sa >= 0 && sb >= 0 -> True
      | otherwise -> k1 * sb + k2 * sa >= 0
  }

