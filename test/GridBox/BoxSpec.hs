module GridBox.BoxSpec where

import GridBox.Box

import Test.Hspec

box :: Box
box = Box 0.5 0.5 0 0 1.0 1.0

vectors :: [(Double, Double)]
vectors = [(1.0,1.0),(1.0,0.0),(0.0,0.0),(0.0,1.0),(1.0,1.0)]

spec :: Spec
spec = do
  describe "makeVectors" $ do
    it "should create vectors from box" $
      makeVectors box `shouldBe` vectors

  describe "makeBox" $ do
    it "should create box" $
      let
        created = makeBox 0.5 0.5 0 0 (1.0, 1.0)
      in
        created `shouldBe` box

  describe "fromBox" $ do
    it "should convert from box to (row, col)" $
      fromBox box `shouldBe` (0, 0)
