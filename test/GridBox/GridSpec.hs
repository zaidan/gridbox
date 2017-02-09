module GridBox.GridSpec where

import GridBox.Grid (createGrid, Grid)
import GridBox.Box (Box(..))

import Test.Hspec

box :: Box
box = Box 0.5 0.5 0 0 1.0 1.0

grid :: Grid
grid = createGrid (1.0, 1.0) 1.0 1.0 0 0 

spec :: Spec
spec =
  describe "createGrid" $ do
    it "shoult create grid" $
      grid `shouldBe` expected
        where expected = [[box]]
