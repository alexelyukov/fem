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
    drawRectangle (V2 500 500, V2 600 700) 3 (PixelRGBA8 0xFF 0x00 0x00 255) Solid
    fillRectangle (V2 500 500, V2 600 700) (PixelRGBA8 0xAA 0x00 0x00 255)
    drawCircle (V2 1000 1000, 200) 3 (PixelRGBA8 0xFF 0x00 0x00 255)
    fillCircle (V2 1000 1000, 200) (PixelRGBA8 0x00 0x00 0xFC 255)
    drawTriangle (V2 700 100, V2 300 100, V2 1800 1100) 3 (PixelRGBA8 0xFF 0x00 0x00 255) Solid
    fillTriangle (V2 700 100, V2 300 100, V2 1800 1100) (PixelRGBA8 0xFF 0x22 0xAA 255)
    drawTriangles [(V2 1100 100, V2 1800 100, V2 1800 1100), (V2 1200 100, V2 1900 100, V2 1900 1200)] 3 (PixelRGBA8 0xFF 0x00 0x00 255) Solid
    fillTriangles [(V2 1100 100, V2 1800 100, V2 1800 1100), (V2 1200 100, V2 1900 100, V2 1900 1200)] (PixelRGBA8 0x22 0x22 0x35 255)
