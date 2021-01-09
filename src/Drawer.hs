module Drawer (
  drawBackground,
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
data LineType = Dashed | Solid deriving (Eq, Ord, Show)

drawBackground :: Drawing PixelRGBA8 () -> Image PixelRGBA8
drawBackground = 
  let backgroundColor = PixelRGBA8 255 255 255 255
      drawColor       = PixelRGBA8 0x00 0x00 0x00 255
      width           = 2000
      height          = 2000
  in renderDrawing width height backgroundColor . withTexture (uniformTexture drawColor)

drawLines :: [(V2 Float, V2 Float)] -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawLines = mconcat . map drawLine

drawLine :: (V2 Float, V2 Float) -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawLine (point0, point1) size color linetype = 
  withTexture (uniformTexture color) $
  case linetype of {
    Solid -> stroke size JoinRound (CapRound, CapRound) $ line point0 point1;
    Dashed -> dashedStroke [5, 10, 5] size JoinRound (CapRound, CapRound) $ line point0 point1
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
