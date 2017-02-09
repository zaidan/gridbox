module GridBox.PositionSpec where

import GridBox.Box (Box(..))
import GridBox.Position (fromBox)

import Test.Hspec

box :: Box
box = Box 0.5 0.5 0 0 1.0 1.0

spec :: Spec
spec = do
  describe "fromBox" $ do
    it "should convert from box to (row, col)" $
      fromBox box `shouldBe` (0, 0)
