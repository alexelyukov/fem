module Main where

import qualified Delaunay
import Codec.Picture ( PixelRGBA8(PixelRGBA8), writePng )
import Geometry
import Drawer
import Triangle
import Node
import System.Random ( mkStdGen )
import Generator
import Graphics.Rasterific

main :: IO ()
main = do
  let generator = mkStdGen 10
      geometry = getGeometry
      generatedPoints = take 10 $ generatePoints generator geometry []

      triangulation0 = Delaunay.run [getSuperTriangle] (geometry2nodes geometry)
      triangulation1 = filter (\t -> isInGeometry (getMedian t) geometry) triangulation0
      triangulation2 = Delaunay.run triangulation1 generatedPoints

      badTriangles = getBadTriangles (Node 646.44666 646.44653) triangulation0

      test1 = isIntoAroundCircle (Node 646.44666 646.44653) (Triangle (Node 500.0 999.99994,Node 1000.0 1500.0,Node 1353.5533 1353.5533))
  print $ show $ geometry2nodes geometry
  print $ show $ badTriangles
  print $ show $ test1
  writePng "image.png" . drawBackground $ do
    -- drawGeometry geometry
    drawPoints (fromNodes $ geometry2nodes geometry) 3 (PixelRGBA8 0xFF 0x00 0x00 255)
    drawPoint (fromNode (Node 646.44666 646.44653)) 5 (PixelRGBA8 0xFF 0x00 0xFF 255)
    drawTriangles (fromTriangles [
        Triangle (Node 500.0 999.99994,Node 1000.0 (-500.0),Node 1353.5533 1353.5533),
        Triangle (Node 500.0 999.99994,Node 1000.0 1500.0,Node 646.44666 1353.5533),
        Triangle (Node 500.0 999.99994,Node (-500.0) 1700.0,Node 1000.0 (-500.0))
      ]) 5 (PixelRGBA8 0xFF 0x00 0xFF 255) Solid
    drawTriangle (fromNode (Node 500.0 999.99994),fromNode (Node 1000.0 1500.0),fromNode (Node 1353.5533 1353.5533)) 5 (PixelRGBA8 0xFF 0xFF 0x00 255) Solid
    drawCircle (test111 (Triangle (Node 500.0 999.99994,Node 1000.0 1500.0,Node 1353.5533 1353.5533))) 3 (PixelRGBA8 0xFF 0x00 0xFF 255)
    drawTriangulation triangulation0 1 2 (PixelRGBA8 0x00 0x00 0x00 255) (PixelRGBA8 0xFF 0x00 0x00 255) Solid

test111 :: Triangle -> (V2 Float, Float)
test111 (Triangle (Node ax ay, Node bx by, Node cx cy)) =
  let d = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
      centerX = ((ax ^ 2 + ay ^ 2) * (by - cy) + (bx ^ 2 + by ^ 2) * (cy - ay) + (cx ^ 2 + cy ^ 2) * (ay - by)) / d
      centerY = ((ax ^ 2 + ay ^ 2) * (cx - bx) + (bx ^ 2 + by ^ 2) * (ax - cx) + (cx ^ 2 + cy ^ 2) * (bx - ax)) / d
      a = sqrt ((bx - ax) ^ 2 + (by - ay) ^ 2)
      b = sqrt ((cx - bx) ^ 2 + (cy - by) ^ 2)
      c = sqrt ((ax - cx) ^ 2 + (ay - cy) ^ 2)
      p = (a + b + c) / 2
      r = a * b * c / (4 * sqrt (p * (p - a) * (p - b) * (p - c)))
  in (V2 centerX centerY, r)