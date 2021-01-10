module DrawerSpec (
  spec,
) where

import Test.Hspec
import Codec.Picture(writePng, PixelRGBA8( .. ))
import Drawer
import Graphics.Rasterific

spec :: Spec
spec =
  describe "getCircleAroundTriangle" $ do
  it "(Point2d 0 0) (Point2d 0 4) (Point2d 4 0) -> True" $
    writePng "image.png" . drawBackground $ testDrawer

testDrawer :: Drawing PixelRGBA8 ()
testDrawer = do
  drawPoint (V2 100 100) 1 (PixelRGBA8 0x00 0x00 0x00 255)
  drawPoint (V2 200 100) 3 (PixelRGBA8 0x00 0x00 0xFF 255)
  drawPoint (V2 300 100) 5 (PixelRGBA8 0x00 0xFF 0x00 255)
  drawPoint (V2 400 100) 7 (PixelRGBA8 0x00 0xFF 0xFF 255)
  drawPoint (V2 500 100) 10 (PixelRGBA8 0xFF 0x00 0x00 255)

  drawPoints [V2 700 100, V2 800 100, V2 900 100, V2 1000 100, V2 1100 100] 5 (PixelRGBA8 0x00 0x00 0x00 255)

  drawLine (Line (V2 100 200) (V2 200 200)) 1 (PixelRGBA8 0x00 0x00 0x00 255) Solid
  drawLine (Line (V2 300 200) (V2 400 200)) 2 (PixelRGBA8 0x00 0x00 0xFF 255) Dashed
  drawLine (Line (V2 500 200) (V2 600 200)) 3 (PixelRGBA8 0x00 0xFF 0x00 255) Solid
  drawLine (Line (V2 700 200) (V2 800 200)) 4 (PixelRGBA8 0x00 0xFF 0xFF 255) Dashed
  drawLine (Line (V2 900 200) (V2 1000 200)) 5 (PixelRGBA8 0xFF 0x00 0x00 255) Solid

  drawLines [
    Line (V2 1100 200) (V2 1200 200),
    Line (V2 1300 200) (V2 1400 200),
    Line (V2 1500 200) (V2 1600 200)] 5 (PixelRGBA8 0x00 0x00 0x00 255) Solid

  drawRectangle (V2 100 300, V2 200 400) 1 (PixelRGBA8 0x00 0x00 0x00 255) Solid
  drawRectangle (V2 300 300, V2 350 400) 3 (PixelRGBA8 0x00 0x00 0xFF 255) Dashed
  drawRectangle (V2 450 300, V2 600 400) 5 (PixelRGBA8 0x00 0xFF 0x00 255) Solid

  drawRectangle (V2 700 300, V2 800 400) 5 (PixelRGBA8 0x00 0xFF 0xFF 255) Solid
  fillRectangle (V2 700 300, V2 800 400) (PixelRGBA8 0xAA 0xAA 0xAA 255)

  drawRectangle (V2 900 300, V2 1000 400) 5 (PixelRGBA8 0xFF 0x00 0x00 255) Solid
  fillRectangle (V2 900 300, V2 1000 400) (PixelRGBA8 0xCC 0xCC 0xCC 255)
  drawPoints [V2 900 300, V2 1000 300, V2 1000 400, V2 900 400] 5 (PixelRGBA8 0x00 0x00 0x00 255)

  drawCircle (V2 150 550, 50) 1 (PixelRGBA8 0x00 0x00 0x00 255)
  drawCircle (V2 350 550, 50) 3 (PixelRGBA8 0x00 0x00 0xFF 255)
  drawCircle (V2 550 550, 50) 5 (PixelRGBA8 0x00 0xFF 0x00 255)

  drawCircle (V2 750 550, 50) 5 (PixelRGBA8 0x00 0xFF 0xFF 255)
  fillCircle (V2 750 550, 50) (PixelRGBA8 0xAA 0xAA 0xAA 255)

  drawCircle (V2 950 550, 50) 5 (PixelRGBA8 0xFF 0x00 0x00 255)
  fillCircle (V2 950 550, 50) (PixelRGBA8 0xCC 0xCC 0xCC 255)
  drawPoint (V2 950 550) 5 (PixelRGBA8 0x00 0x00 0x00 255)

  drawTriangle (V2 100 700, V2 200 700, V2 150 800) 1 (PixelRGBA8 0x00 0x00 0x00 255) Solid
  drawTriangle (V2 350 700, V2 400 800, V2 300 800) 3 (PixelRGBA8 0x00 0x00 0xFF 255) Dashed
  drawTriangle (V2 500 700, V2 600 700, V2 550 800) 5 (PixelRGBA8 0x00 0xFF 0x00 255) Solid

  drawTriangle (V2 750 700, V2 800 800, V2 700 800) 5 (PixelRGBA8 0x00 0xFF 0xFF 255) Dashed
  fillTriangle (V2 750 700, V2 800 800, V2 700 800) (PixelRGBA8 0xAA 0xAA 0xAA 255)

  drawTriangles [(V2 900 700, V2 1000 700, V2 950 800), (V2 1150 700, V2 1200 800, V2 1100 800)] 5 (PixelRGBA8 0xFF 0x00 0x00 255) Solid
  fillTriangles [(V2 900 700, V2 1000 700, V2 950 800), (V2 1150 700, V2 1200 800, V2 1100 800)] (PixelRGBA8 0xCC 0xCC 0xCC 255)
  drawPoints [V2 900 700, V2 1000 700, V2 950 800, V2 1150 700, V2 1200 800, V2 1100 800] 5 (PixelRGBA8 0x00 0x00 0x00 255)

  drawPolygon [V2 100 900, V2 150 950, V2 200 900, V2 200 1000, V2 150 1050, V2 100 1000] 1 (PixelRGBA8 0x00 0x00 0x00 255) Solid
  drawPolygon [V2 300 900, V2 350 950, V2 400 900, V2 400 1000, V2 350 1050, V2 300 1000] 2 (PixelRGBA8 0x00 0x00 0xFF 255) Dashed
  drawPolygon [V2 500 900, V2 550 950, V2 600 900, V2 600 1000, V2 550 1050, V2 500 1000] 3 (PixelRGBA8 0x00 0xFF 0x00 255) Solid

  drawPolygon [V2 700 900, V2 750 950, V2 800 900, V2 800 1000, V2 750 1050, V2 700 1000] 3 (PixelRGBA8 0x00 0xFF 0xFF 255) Solid
  fillPolygon [V2 700 900, V2 750 950, V2 800 900, V2 800 1000, V2 750 1050, V2 700 1000] (PixelRGBA8 0xAA 0xAA 0xAA 255)

  drawPolygons [
    [V2 900 900, V2 950 950, V2 1000 900, V2 1000 1000, V2 950 1050, V2 900 1000],
    [V2 1100 900, V2 1150 950, V2 1200 900, V2 1200 1000, V2 1150 1050, V2 1100 1000]] 3 (PixelRGBA8 0xFF 0x00 0x00 255) Solid
  fillPolygons [
    [V2 900 900, V2 950 950, V2 1000 900, V2 1000 1000, V2 950 1050, V2 900 1000],
    [V2 1100 900, V2 1150 950, V2 1200 900, V2 1200 1000, V2 1150 1050, V2 1100 1000]] (PixelRGBA8 0xCC 0xCC 0xCC 255)
  drawPoints [
    V2 900 900, V2 950 950, V2 1000 900, V2 1000 1000, V2 950 1050, V2 900 1000,
    V2 1100 900, V2 1150 950, V2 1200 900, V2 1200 1000, V2 1150 1050, V2 1100 1000] 5 (PixelRGBA8 0x00 0x00 0x00 255)
