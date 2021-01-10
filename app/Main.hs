module Main where

import Codec.Picture(writePng, PixelRGBA8( .. ))
import Drawer
import Graphics.Rasterific
import Node
import Triangle
import Delaunay

main :: IO ()
main = do
  let initialPoints = generatePoints
      [n1, n2, n3] = [Node 1000 (-500), Node (-500) 1700, Node 2500 1700]
      firstTriangle = [Triangle (n1, n2, n3)]
      triangulation = Delaunay.run firstTriangle initialPoints
      triangulationWithoutSuper = filter (\(Triangle (node1, node2, node3)) -> node1 `notElem` [n1, n2, n3] && node2 `notElem` [n1, n2, n3] && node3 `notElem` [n1, n2, n3]) triangulation
  -- print $ show triangulation
  writePng "image.png" . drawBackground $ do
    drawGeometry
    drawInitialPoints . fromNodes $ initialPoints
    drawTriangles (fromTriangles triangulationWithoutSuper) 1 (PixelRGBA8 0x00 0x00 0x00 255) Solid
    

drawGeometry :: Drawing PixelRGBA8 ()
drawGeometry = do
  drawRectangle (V2 500 500, V2 1500 1500) 5 (PixelRGBA8 0x00 0x00 0x00 255) Solid
  fillRectangle (V2 500 500, V2 1500 1500) (PixelRGBA8 0xDD 0xDD 0xDD 255)

  drawRectangle (V2 700 700, V2 900 1300) 5 (PixelRGBA8 0x00 0x00 0x00 255) Solid
  fillRectangle (V2 700 700, V2 900 1300) (PixelRGBA8 0xFF 0xFF 0xFF 255)

  drawRectangle (V2 1100 700, V2 1300 1300) 5 (PixelRGBA8 0x00 0x00 0x00 255) Solid
  fillRectangle (V2 1100 700, V2 1300 1300) (PixelRGBA8 0xFF 0xFF 0xFF 255)

drawInitialPoints :: [V2 Float] -> Drawing PixelRGBA8 ()
drawInitialPoints points = drawPoints points 3 (PixelRGBA8 0x00 0x00 0x00 255)

generatePoints :: [Node]
generatePoints = [Node (spreadPoints 500 1500 i 50) 500 | i <- [0..50]]
  ++ [Node 1500 (spreadPoints 500 1500 i 50) | i <- [1..49]]
  ++ [Node (spreadPoints 500 1500 i 50) 1500 | i <- [0..50]]
  ++ [Node 500 (spreadPoints 500 1500 i 50) | i <- [1..49]]

  ++ [Node (spreadPoints 700 900 i 10) 700 | i <- [0..10]]
  ++ [Node 900 (spreadPoints 700 1300 i 30) | i <- [1..29]]
  ++ [Node (spreadPoints 700 900 i 10) 1300 | i <- [0..10]]
  ++ [Node 700 (spreadPoints 700 1300 i 30) | i <- [1..29]]

  ++ [Node (spreadPoints 1100 1300 i 10) 700 | i <- [0..10]]
  ++ [Node 1300 (spreadPoints 700 1300 i 30) | i <- [1..29]]
  ++ [Node (spreadPoints 1100 1300 i 10) 1300 | i <- [0..10]]
  ++ [Node 1100 (spreadPoints 700 1300 i 30) | i <- [1..29]]

spreadPoints :: Float -> Float -> Float -> Float -> Float
spreadPoints x1 x2 i n = x1 + (x2 - x1) * (i / n)