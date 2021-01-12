module Main where

import qualified Delaunay
import Codec.Picture ( PixelRGBA8(PixelRGBA8), writePng )
import Geometry
import Drawer
import Triangle
import Node
import System.Random ( mkStdGen )
import Generator

main :: IO ()
main = do
  let generator = mkStdGen 10
      geometry = getGeometry
      -- generatedPoints = take 600 $ generatePoints generator geometry []

      -- triangulation0 = Delaunay.run [getSuperTriangle] (geometry2nodes geometry)
      -- triangulation1 = filter (\t -> isInGeometry (getMedian t) geometry) triangulation0
      -- triangulation2 = Delaunay.run triangulation1 generatedPoints
  -- print $ show $ (head $ reverse generatedPoints)
  writePng "image.png" . drawBackground $ do
    drawGeometry geometry
    -- drawTriangulation triangulation2 1 2 (PixelRGBA8 0x00 0x00 0x00 255) (PixelRGBA8 0xFF 0x00 0x00 255) Solid
