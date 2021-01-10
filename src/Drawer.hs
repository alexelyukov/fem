module Drawer (
  LineType(..),
  drawBackground,

  fillPolygons,
  fillPolygon,

  drawPolygons,
  drawPolygon,

  fillCircle,
  drawCircle,

  fillTriangles,
  fillTriangle,

  drawTriangles,
  drawTriangle,

  fillRectangle,
  drawRectangle,

  drawLines,
  drawLine,

  drawPoints,
  drawPoint,
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

fillPolygons :: [[V2 Float]] -> PixelRGBA8 -> Drawing PixelRGBA8 ()
fillPolygons = mconcat . map fillPolygon

fillPolygon :: [V2 Float] -> PixelRGBA8 -> Drawing PixelRGBA8 ()
fillPolygon (p1:p2:p3:ps) color =
  withTexture (uniformTexture color) . fill . packPoints $ ((p1:p2:p3:ps) ++ [p1])

drawPolygons :: [[V2 Float]] -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawPolygons = mconcat . map drawPolygon

drawPolygon :: [V2 Float] -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawPolygon (p1:p2:p3:ps) = drawLines . packPoints $ ((p1:p2:p3:ps) ++ [p1])

fillCircle :: (V2 Float, Radius) -> PixelRGBA8 -> Drawing PixelRGBA8 ()
fillCircle (pc, r) color =
  withTexture (uniformTexture color) . fill $ circle pc r

drawCircle :: (V2 Float, Radius) -> Size -> PixelRGBA8 -> Drawing PixelRGBA8 ()
drawCircle (pc, r) size color =
  withTexture (uniformTexture color) . stroke size JoinRound (CapRound, CapRound) $ circle pc r

fillTriangles :: [(V2 Float, V2 Float, V2 Float)] -> PixelRGBA8 -> Drawing PixelRGBA8 ()
fillTriangles = mconcat . map fillTriangle

fillTriangle :: (V2 Float, V2 Float, V2 Float) -> PixelRGBA8 -> Drawing PixelRGBA8 ()
fillTriangle (point1, point2, point3) color =
  withTexture (uniformTexture color) . fill . packPoints $ [point1, point2, point3, point1]

drawTriangles :: [(V2 Float, V2 Float, V2 Float)] -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawTriangles = mconcat . map drawTriangle

drawTriangle :: (V2 Float, V2 Float, V2 Float) -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawTriangle (point1, point2, point3) = drawLines . packPoints $ [point1, point2, point3, point1]

fillRectangle :: (V2 Float, V2 Float) -> PixelRGBA8 -> Drawing PixelRGBA8 ()
fillRectangle (V2 x1 y1, V2 x2 y2) color =
  withTexture (uniformTexture color) . fill . packPoints $ [V2 x1 y1, V2 x2 y1, V2 x2 y2, V2 x1 y2, V2 x1 y1]

drawRectangle :: (V2 Float, V2 Float) -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawRectangle (V2 x1 y1, V2 x2 y2) = drawLines . packPoints $ [V2 x1 y1, V2 x2 y1, V2 x2 y2, V2 x1 y2, V2 x1 y1]

drawLines :: [Line] -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawLines = mconcat . map drawLine

drawLine :: Line -> Size -> PixelRGBA8 -> LineType -> Drawing PixelRGBA8 ()
drawLine (Line point1 point2) size color linetype = 
  withTexture (uniformTexture color) (case linetype of {
    Solid -> stroke size JoinRound (CapRound, CapRound) $ line point1 point2;
    Dashed -> dashedStroke [5, 10, 5] size JoinRound (CapRound, CapRound) $ line point1 point2
  })

drawPoints :: [V2 Float] -> Size -> PixelRGBA8 -> Drawing PixelRGBA8 ()
drawPoints = mconcat . map drawPoint

drawPoint :: V2 Float -> Size -> PixelRGBA8 -> Drawing PixelRGBA8 ()
drawPoint point size color = withTexture (uniformTexture color) . fill $ circle point size

packPoints :: [V2 Float] -> [Line]
packPoints [] = []
packPoints [_] = []
packPoints (p1:p2:ps) = Line p1 p2 : packPoints (p2:ps)