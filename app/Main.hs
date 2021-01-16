module Main where

import Control.Monad (msum)
import Happstack.Server (addHeader, setHeader, Response, Response, WebMonad(finishWith), result, ToMessage(toResponse), ServerPartT, defaultBodyPolicy, decodeBody, Method(OPTIONS, GET, POST), look, dir, method, nullConf, ok, simpleHTTP)
import qualified Controllers.ThermalFieldRectangleController as ThermalFieldRectangleController
import qualified Controllers.ThermalFieldTubeController as ThermalFieldTubeController
import qualified Controllers.ElectrostaticFieldSkinController as ElectrostaticFieldSkinleController
import qualified Controllers.ElectromagneticFieldMagnetController as ElectromagneticFieldMagnetController
import qualified Controllers.ElectrostaticFieldCordController as ElectrostaticFieldCordController
import qualified Controllers.ElectromagneticFieldMagnetController as ElectromagneticFieldTransformerController
import qualified Controllers.ElectrodynamicSystemController as ElectrodynamicSystemController
import Config

main :: IO ()
main = simpleHTTP serverConf $ msum $
        getRoute ThermalFieldRectangleController.url ThermalFieldRectangleController.handler
    ++  getRoute ThermalFieldTubeController.url ThermalFieldTubeController.handler
    ++  getRoute ElectrostaticFieldSkinleController.url ElectrostaticFieldSkinleController.handler
    ++  getRoute ElectrostaticFieldCordController.url ElectrostaticFieldCordController.handler
    ++  getRoute ElectromagneticFieldMagnetController.url ElectromagneticFieldMagnetController.handler
    ++  getRoute ElectromagneticFieldTransformerController.url ElectromagneticFieldTransformerController.handler
    ++  getRoute ElectrodynamicSystemController.url ElectrodynamicSystemController.handler

getRoute :: String -> (Int -> String) -> [ServerPartT IO String]
getRoute url handler = [
    dir url $
    do  method POST
        -- decodeBody (defaultBodyPolicy "." 0 65536 65536 )
        -- geometry <- look "geometry"
        finishWith $ addHeaders $ result 200 $ handler 12,
    dir url $
    do  method OPTIONS
        finishWith $ addHeaders $ result 200 "OK"
    ]

addHeaders :: Response -> Response
addHeaders =  addHeader "Access-Control-Allow-Origin" "*"
            . addHeader "Access-Control-Allow-Methods" "*"
            . addHeader "Access-Control-Allow-Headers" "*"
            . addHeader "Content-Type" "text/html; charset=utf-8"

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