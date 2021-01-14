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
      generatedPoints = take 3 $ generatePoints generator geometry []

      triangulation = Delaunay.run getInitialTriangulation generatedPoints
      cleanTriangulation = filter (\t -> isInGeometry (getMedian t) geometry) triangulation
  print $ show $ generatedPoints
  writePng "image.png" . drawBackground $ do
    drawGeometry geometry
    -- drawPoints (fromNodes $ geometry2nodes geometry) 3 (PixelRGBA8 0xFF 0x00 0x00 255)
    drawTriangulation triangulation 1 2 (PixelRGBA8 0x00 0x00 0x00 255) (PixelRGBA8 0xFF 0x00 0x00 255) Solid
