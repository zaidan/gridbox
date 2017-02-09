module GridBox.BoxSpec where

import GridBox.Box (Box(..), makeBox)

import Test.Hspec

box :: Box
box = Box 0.5 0.5 0 0 1.0 1.0

spec :: Spec
spec = do
  describe "makeBox" $ do
    it "should create box" $
      created `shouldBe` box
        where
          created = makeBox 0.5 0.5 0 0 (1.0, 1.0)
