module Main where

import Control.Monad (msum)
import Happstack.Server (defaultBodyPolicy, decodeBody, Method(GET, POST), look, dir, method, nullConf, ok, simpleHTTP)
import qualified Enums.EApiUrl as EApiUrl
import qualified Controllers.ThermalFieldRectangleController as ThermalFieldRectangleController
import qualified Controllers.ThermalFieldTubeController as ThermalFieldTubeController
import qualified Controllers.ElectrostaticFieldSkinController as ElectrostaticFieldSkinleController
import qualified Controllers.ElectromagneticFieldMagnetController as ElectromagneticFieldMagnetController
import qualified Controllers.ElectrostaticFieldCordController as ElectrostaticFieldCordController
import qualified Controllers.ElectromagneticFieldMagnetController as ElectromagneticFieldTransformerController
import qualified Controllers.ElectrodynamicSystemController as ElectrodynamicSystemController
import Config
import qualified Happstack.Server as Happstack.Server.Internal.MessageWrap

main :: IO ()
main = simpleHTTP serverConf $ msum
  [
    dir (EApiUrl.getUrl EApiUrl.ThermalFieldRectangle) $
      do method POST
         decodeBody (defaultBodyPolicy "." 0 65536 65536)
         geometry <- look "geometry"
         ok $ ThermalFieldRectangleController.handler (read geometry :: Int),
    dir (EApiUrl.getUrl EApiUrl.ThermalFieldTube) $
      do method POST
         decodeBody (defaultBodyPolicy "." 0 65536 65536)
         geometry <- look "geometry"
         ok $ ThermalFieldTubeController.handler (read geometry :: Int),
    dir (EApiUrl.getUrl EApiUrl.ElectrostaticFieldSkin) $
      do method POST
         decodeBody (defaultBodyPolicy "." 0 65536 65536)
         geometry <- look "geometry"
         ok $ ElectrostaticFieldSkinleController.handler (read geometry :: Int),
    dir (EApiUrl.getUrl EApiUrl.ElectrostaticFieldCord) $
      do method POST
         decodeBody (defaultBodyPolicy "." 0 65536 65536)
         geometry <- look "geometry"
         ok $ ElectrostaticFieldCordController.handler (read geometry :: Int),
    dir (EApiUrl.getUrl EApiUrl.ElectromagneticFieldMagnet) $
      do method POST
         decodeBody (defaultBodyPolicy "." 0 65536 65536)
         geometry <- look "geometry"
         ok $ ElectromagneticFieldMagnetController.handler (read geometry :: Int),
    dir (EApiUrl.getUrl EApiUrl.ElectromagneticFieldTransformer) $
      do method POST
         decodeBody (defaultBodyPolicy "." 0 65536 65536)
         geometry <- look "geometry"
         ok $ ElectromagneticFieldTransformerController.handler (read geometry :: Int),
    dir (EApiUrl.getUrl EApiUrl.ElectrodynamicSystem) $
      do method POST
         decodeBody (defaultBodyPolicy "." 0 65536 65536)
         geometry <- look "geometry"
         ok $ ElectrodynamicSystemController.handler (read geometry :: Int)
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