module Controllers.Controller1 (
  resultController
) where

resultController :: Int -> Int -> String
resultController a b = (++) "a + b = " $ show $ a + b