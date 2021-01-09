module DelaunaySpec (
  spec,
) where

import Test.Hspec
import Delaunay
import Node
import Triangle
import Graphics.Rasterific

spec :: Spec
spec =
  describe "проверка триангуляции" $ do
  it "тупоугольные треугольники" $
    check (Node 0 (1 * 1.1)) (Triangle (Node 1 (2 * 1.1), Node 2 (1 * 1.1), Node 1 (0 * 1.1))) `shouldBe` False
  it "прямоугольные треугольники" $
    check (Node 0 (1 * 1.0)) (Triangle (Node 1 (2 * 1.0), Node 2 (1 * 1.0), Node 1 (0 * 1.0))) `shouldBe` True
  it "остроугольные треугольники" $
    check (Node 0 (1 * 0.9)) (Triangle (Node 1 (2 * 0.9), Node 2 (1 * 0.9), Node 1 (0 * 0.9))) `shouldBe` True