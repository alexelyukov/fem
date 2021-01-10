module DelaunaySpec (
  spec,
) where

import Test.Hspec
import Delaunay
import Graphics.Rasterific

spec :: Spec
spec =
  describe "проверка триангуляции" $ do
  it "тупоугольные треугольники" $
    check (V2 1 (2 * 1.1), V2 2 (1 * 1.1), V2 1 (0 * 1.1)) (V2 0 (1 * 1.1)) `shouldBe` False
  it "прямоугольные треугольники" $
    check (V2 1 (2 * 1.0), V2 2 (1 * 1.0), V2 1 (0 * 1.0)) (V2 0 (1 * 1.0)) `shouldBe` True
  it "остроугольные треугольники" $
    check (V2 1 (2 * 0.9), V2 2 (1 * 0.9), V2 1 (0 * 0.9)) (V2 0 (1 * 0.9)) `shouldBe` True