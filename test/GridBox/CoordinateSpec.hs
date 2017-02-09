module GridBox.CoordinateSpec where

import GridBox.Box (Box(..))
import GridBox.Coordinate (fromBox)

import Test.Hspec

box :: Box
box = Box 0.5 0.5 0 0 1.0 1.0

coordinates :: [(Double, Double)]
coordinates = [(1.0,1.0),(1.0,0.0),(0.0,0.0),(0.0,1.0),(1.0,1.0)]

spec :: Spec
spec = do
  describe "fromBox" $ do
    it "should create list of coordinates from box" $
      fromBox box `shouldBe` coordinates
