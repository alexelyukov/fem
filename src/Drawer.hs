module Drawer (
  drawBackground,
  fillCircle,
  drawCircle,
  fillTriangles,
  drawTriangles,
  fillTriangle,
  drawTriangle,
  fillRectangle,
  drawRectangle,
  drawLines,
  drawLine,
  drawPoints,
  drawPoint,
  LineType(..),
) where

import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture(PixelRGBA8( .. ), Image)

type Size = Float
type Radius = Float
data LineType = Dashed | Solid deriving (Eq, Ord, Show)

drawBackground :: Drawing PixelRGBA8 () -> Image PixelRGBA8
drawBackground = 
  let backgroundColor = PixelRGBA8 255 255 255 255
      drawColor       = PixelRGBA8 0x00 0x00 0x00 255
      width           = 2000
      height          = 2000
  in renderDrawing width height backgroundColor . withTexture (uniformTexture drawColor)

fillCircle :: (V2 Float, Radius) -> PixelRGBA8 -> Drawing PixelRGBA8 ()
fillCircle (pc, r) color =
  withTexture (uniformTexture color) $ fill $ circle pc r

drawCircle :: (V2 Float, Radius) -> Size -> PixelRGBA8 -> Drawing PixelRGBA8 ()
drawCircle (pc, r) size color =
  withTexture (uniformTexture color) $ stroke size JoinRound (CapRound, CapRound) $ circle pc r

fillTriangles :: [(V2 Float, V2 Float, V2 Float)] -> PixelRGBA8 -> Drawing PixelRGBA8 ()
fillTriangles = mconcat . map fillTriangle

drawTriangles :: [(V2 Float, V2 Float, V2 Float)] -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawTriangles = mconcat . map drawTriangle

fillTriangle :: (V2 Float, V2 Float, V2 Float) -> PixelRGBA8 -> Drawing PixelRGBA8 ()
fillTriangle (point1, point2, point3) color =
  withTexture (uniformTexture color) $ fill [Line point1 point2, Line point2 point3, Line point3 point1]

drawTriangle :: (V2 Float, V2 Float, V2 Float) -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawTriangle (point1, point2, point3) =
  drawLines [(point1, point2), (point2, point3), (point3, point1)]

fillRectangle :: (V2 Float, V2 Float) -> PixelRGBA8 -> Drawing PixelRGBA8 ()
fillRectangle (V2 x1 y1, V2 x2 y2) color =
  withTexture (uniformTexture color) $ fill $ rectangle (V2 x1 y1) (x2-x1) (y2-y1)

drawRectangle :: (V2 Float, V2 Float) -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawRectangle (V2 x1 y1, V2 x2 y2) =
  drawLines [(V2 x1 y1, V2 x2 y1), (V2 x2 y1, V2 x2 y2), (V2 x2 y2, V2 x1 y2), (V2 x1 y2, V2 x1 y1)]

drawLines :: [(V2 Float, V2 Float)] -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawLines = mconcat . map drawLine

drawLine :: (V2 Float, V2 Float) -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawLine (point1, point2) size color linetype = 
  withTexture (uniformTexture color) $
  case linetype of {
    Solid -> stroke size JoinRound (CapRound, CapRound) $ line point1 point2;
    Dashed -> dashedStroke [5, 10, 5] size JoinRound (CapRound, CapRound) $ line point1 point2
  }

drawPoints :: [V2 Float] -> Size -> PixelRGBA8 -> Drawing PixelRGBA8 ()
drawPoints = mconcat . map drawPoint

drawPoint :: V2 Float -> Size -> PixelRGBA8 -> Drawing PixelRGBA8 ()
drawPoint point size color = withTexture (uniformTexture color) $ fill $ circle point size

-- drawPoint (V2 100 100) 5 (PixelRGBA8 0x00 0x00 0x00 255)
-- drawPoints [V2 300 300, V2 400 400] 5 (PixelRGBA8 0x00 0xFF 0x00 255)
-- drawLine (V2 300 300, V2 400 400) 1 (PixelRGBA8 0xFF 0x00 0x00 255) Solid
-- drawLine (V2 300 300, V2 400 300) 1 (PixelRGBA8 0xFF 0x00 0x00 255) Dashed
-- drawLines [(V2 600 600, V2 700 700), (V2 800 800, V2 900 900)] 3 (PixelRGBA8 0xFF 0x00 0x00 255) Dashed
-- drawRectangle (V2 500 500, V2 500 600, V2 600 600, V2 600 500) 3 (PixelRGBA8 0xFF 0x00 0x00 255) Solid
-- drawCircle (V2 1000 1000, 200) 3 (PixelRGBA8 0xFF 0x00 0x00 255)
-- drawTriangle (V2 700 100, V2 300 100, V2 1800 1100) 3 (PixelRGBA8 0xFF 0x00 0x00 255) Solid
-- drawTriangles [(V2 1100 100, V2 1800 100, V2 1800 1100), (V2 1200 100, V2 1900 100, V2 1900 1200)] 3 (PixelRGBA8 0xFF 0x00 0x00 255) Solid
