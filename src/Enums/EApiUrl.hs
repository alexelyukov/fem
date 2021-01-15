module Enums.EApiUrl (
  Urls (..),
  getUrl
) where

data Urls = ThermalFieldRectangle
  | ThermalFieldTube
  | ElectrostaticFieldSkin
  | ElectrostaticFieldCord
  | ElectromagneticFieldMagnet
  | ElectromagneticFieldTransformer
  | ElectrodynamicSystem deriving (Show, Eq)

getUrl :: Urls -> String
getUrl ThermalFieldRectangle            = "thermal-field-rectangle"
getUrl ThermalFieldTube                 = "thermal-field-tube"
getUrl ElectrostaticFieldSkin           = "electrostatic-field-skin"
getUrl ElectrostaticFieldCord           = "electrostatic-field-cord"
getUrl ElectromagneticFieldMagnet       = "electromagnetic-field-magnet"
getUrl ElectromagneticFieldTransformer  = "electromagnetic-field-transformer"
getUrl ElectrodynamicSystem             = "electrodynamic-system"
