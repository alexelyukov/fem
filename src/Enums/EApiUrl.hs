module Enums.EApiUrl (
  Urls (Controller1, Controller2),
  getUrl
) where

data Urls = Controller1 | Controller2 deriving (Show, Eq)

getUrl :: Urls -> String
getUrl Controller1 = "controller1"
getUrl Controller2 = "controller2"