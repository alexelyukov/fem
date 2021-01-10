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
    [whereIsPoint getTriangle (Node 0 5), whereIsPoint getTriangle (Node 5 0), whereIsPoint getTriangle (Node 0 0)]
      `shouldBe` [OnNode, OnNode, OnNode]
  it "точка лежит на ребре" $
    [whereIsPoint getTriangle (Node 0 3), whereIsPoint getTriangle (Node 2 0), whereIsPoint getTriangle (Node 2.5 2.5)]
      `shouldBe` [OnEdge, OnEdge, OnEdge]
  it "точка внутри треугольника" $
    [whereIsPoint getTriangle (Node 0.1 3), whereIsPoint getTriangle (Node 2 1)]
      `shouldBe` [In, In]
  it "точка снаружи треугольника" $
    [whereIsPoint getTriangle (Node (-0.1) 3), whereIsPoint getTriangle (Node 2 (-1))]
      `shouldBe` [Out, Out]

getTriangle :: Triangle
getTriangle = Triangle (Node 0 5, Node 5 0, Node 0 0) (Nothing, Nothing, Nothing)