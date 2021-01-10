module TriangleSpec (
  spec,
) where

import Test.Hspec
import Triangle
import Node
import Graphics.Rasterific

spec :: Spec
spec =
  describe "проверка попадания точки" $ do
  it "точка совпадает с узлом" $
    [whereIsPoint (Node 0 5) getTriangle, whereIsPoint (Node 5 0) getTriangle, whereIsPoint (Node 0 0) getTriangle]
      `shouldBe` [OnNode, OnNode, OnNode]
  it "точка лежит на ребре" $
    [whereIsPoint (Node 0 3) getTriangle, whereIsPoint (Node 2 0) getTriangle, whereIsPoint (Node 2.5 2.5) getTriangle]
      `shouldBe` [OnEdge, OnEdge, OnEdge]
  it "точка внутри треугольника" $
    [whereIsPoint (Node 0.1 3) getTriangle, whereIsPoint (Node 2 1) getTriangle]
      `shouldBe` [In, In]
  it "точка снаружи треугольника" $
    [whereIsPoint (Node (-0.1) 3) getTriangle, whereIsPoint (Node 2 (-1)) getTriangle]
      `shouldBe` [Out, Out]

getTriangle :: Triangle
getTriangle = Triangle (Node 0 5, Node 5 0, Node 0 0)