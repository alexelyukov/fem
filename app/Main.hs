module Main where

import Control.Monad (msum)
import Happstack.Server (Method(GET, POST), look, dir, method, nullConf, ok, simpleHTTP)
import qualified Config as C
import qualified Enums.EApiUrl as U
import qualified Controllers.Controller1 as C1
import qualified Controllers.Controller2 as C2

main :: IO ()
main = simpleHTTP C.serverConf $ msum
  [
    dir (U.getUrl U.Controller1) $ do method GET
                                      a <- look "a"
                                      b <- look "b"
                                      ok $ C1.resultController (read a :: Int) (read b :: Int),
    dir (U.getUrl U.Controller2) $ do method POST
                                      a <- look "a"
                                      b <- look "b"
                                      ok $ C2.resultController (read a :: Int) (read b :: Int)
  ]


-- module Main where

-- import qualified Delaunay
-- import Codec.Picture ( PixelRGBA8(PixelRGBA8), writePng )
-- import Geometry
-- import Drawer
-- import Triangle
-- import Node
-- import System.Random ( mkStdGen )
-- import Generator
-- import Graphics.Rasterific

-- main :: IO ()
-- main = do
--   let generator = mkStdGen 10
--       geometry = getGeometry
--       generatedPoints = take 3 $ generatePoints generator geometry []

--       triangulation = Delaunay.run getInitialTriangulation generatedPoints
--       cleanTriangulation = filter (\t -> isInGeometry (getMedian t) geometry) triangulation
--   print $ show $ generatedPoints
--   writePng "image.png" . drawBackground $ do
--     drawGeometry geometry
--     drawPoints (fromNodes $ geometry2nodes geometry) 3 (PixelRGBA8 0xFF 0x00 0x00 255)
--     drawTriangulation triangulation 1 2 (PixelRGBA8 0x00 0x00 0x00 255) (PixelRGBA8 0xFF 0x00 0x00 255) Solid