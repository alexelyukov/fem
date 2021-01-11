module GrahamSpec (
  spec,
) where

import Test.Hspec
import Node
import Triangle
import Graham

spec :: Spec
spec =
  describe "проверка алгоритма Грэхэма" $ do
  it "точка справа от отрезка" $ [
    leftOrRight (Node (-3) (-4)) (Node 0 0) (Node 1 1),
    leftOrRight (Node 3 2) (Node 0 0) (Node 1 1),
    leftOrRight (Node 1 0) (Node 0 0) (Node 1 1),
    leftOrRight (Node 0 2) (Node 1 1) (Node 0 0),
    leftOrRight (Node (-1) 0) (Node 1 1) (Node 0 0)] `shouldBe` [LT, LT, LT, LT, LT]
  it "точка слева от отрезка" $ [
    leftOrRight (Node (-3) (-4)) (Node 1 1) (Node 0 0),
    leftOrRight (Node 3 2) (Node 1 1) (Node 0 0),
    leftOrRight (Node 1 0) (Node 1 1) (Node 0 0),
    leftOrRight (Node 0 2) (Node 0 0) (Node 1 1),
    leftOrRight (Node (-1) 0) (Node 0 0) (Node 1 1)] `shouldBe` [GT, GT, GT, GT, GT]
  it "точка принадлежит отрезку отрезка" $ [
    leftOrRight (Node (-3) (-3)) (Node 1 1) (Node 0 0),
    leftOrRight (Node 3 3) (Node 1 1) (Node 0 0)] `shouldBe` [EQ, EQ]
  it "нахождение точки с минимальной абциссой" $ [
    step1 [Node (-3) (-3), Node 3 3, Node 0 0],
    step1 [Node (-1) 0, Node 1 0, Node 10 10]] `shouldBe` [Node (-3) (-3), Node (-1) 0]
  it "сортировка точек 1" $ step2 (Node 0 0) [
      Node 0 3, Node 0 2, Node 1 (-1), Node 1 1, Node 0 (-1), Node 0 (-4)
    ] `shouldBe` [Node 0 (-1), Node 0 (-4), Node 1 (-1), Node 1 1, Node 0 3, Node 0 2]
  it "сортировка точек 2" $ step2 (Node 0 0) [
      Node 0 3, Node 0 2, Node 1 (-1), Node 1 1, Node 0 (-4), Node 0 (-1)
    ] `shouldBe` [Node 0 (-4), Node 0 (-1), Node 1 (-1), Node 1 1, Node 0 3, Node 0 2]
