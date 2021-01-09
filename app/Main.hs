module Main where

import Codec.Picture(writePng, PixelRGBA8( .. ))
import Drawer
import Graphics.Rasterific

main :: IO ()
main = do
  writePng "image.png" $ drawBackground $ do
    drawPoint (V2 100 100) 5 (PixelRGBA8 0x00 0x00 0x00 255)
    drawPoints [V2 300 300, V2 400 400] 5 (PixelRGBA8 0x00 0xFF 0x00 255)
    drawLine (V2 300 300, V2 400 400) 1 (PixelRGBA8 0xFF 0x00 0x00 255) Solid
    drawLine (V2 300 300, V2 400 300) 1 (PixelRGBA8 0xFF 0x00 0x00 255) Dashed
    drawLines [(V2 600 600, V2 700 700), (V2 800 800, V2 900 900)] 3 (PixelRGBA8 0xFF 0x00 0x00 255) Dashed
