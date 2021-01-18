module Main where

import Control.Monad (msum)
import Happstack.Server (RqBody(unBody), takeRequestBody, ServerMonad(askRq), addHeader, setHeader, Response, Response, WebMonad(finishWith), result, ToMessage(toResponse), ServerPartT, defaultBodyPolicy, decodeBody, Method(OPTIONS, GET, POST), look, dir, method, nullConf, ok, simpleHTTP)
import qualified Controllers.ThermalFieldRectangleController as ThermalFieldRectangleController
import qualified Controllers.ThermalFieldTubeController as ThermalFieldTubeController
import qualified Controllers.ElectrostaticFieldSkinController as ElectrostaticFieldSkinleController
import qualified Controllers.ElectromagneticFieldMagnetController as ElectromagneticFieldMagnetController
import qualified Controllers.ElectrostaticFieldCordController as ElectrostaticFieldCordController
import qualified Controllers.ElectromagneticFieldTransformerController as ElectromagneticFieldTransformerController
import qualified Controllers.ElectrodynamicSystemController as ElectrodynamicSystemController
import Config
import Control.Monad.IO.Class
import Types
import Data.Aeson

main :: IO ()
main = simpleHTTP serverConf $ msum $
        getRoute ThermalFieldRectangleController.url ThermalFieldRectangleController.handler
    ++  getRoute ThermalFieldTubeController.url ThermalFieldTubeController.handler
    ++  getRoute ElectrostaticFieldSkinleController.url ElectrostaticFieldSkinleController.handler
    ++  getRoute ElectrostaticFieldCordController.url ElectrostaticFieldCordController.handler
    ++  getRoute ElectromagneticFieldMagnetController.url ElectromagneticFieldMagnetController.handler
    ++  getRoute ElectromagneticFieldTransformerController.url ElectromagneticFieldTransformerController.handler
    ++  getRoute ElectrodynamicSystemController.url ElectrodynamicSystemController.handler

getRoute :: String -> (Geometry -> String) -> [ServerPartT IO String]
getRoute url handler = [
    dir url $
    do  method POST
        req  <- askRq
        body <- liftIO $ takeRequestBody req
        case body of 
            Just rqbody ->
                case decode (unBody rqbody) :: Maybe TriangulationRequest of
                    Just TriangulationRequest {geometry = geometry} -> finishWith $ addHeaders $ result 200 $ handler geometry
                    Nothing -> finishWith $ addHeaders $ result 400 "Bad Request"
            Nothing -> finishWith $ addHeaders $ result 400 "Bad Request",
    dir url $
    do  method OPTIONS
        finishWith $ addHeaders $ result 200 "OK"
    ]

addHeaders :: Response -> Response
addHeaders =  addHeader "Access-Control-Allow-Origin" "*"
            . addHeader "Access-Control-Allow-Methods" "*"
            . addHeader "Access-Control-Allow-Headers" "*"
            . addHeader "Content-Type" "text/html; charset=utf-8"
