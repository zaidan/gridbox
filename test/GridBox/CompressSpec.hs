module GridBox.CompressSpec where

import GridBox.Grid (createGrid, Grid)
import GridBox.Compress (compressGrid, CompressedGrid)

import Test.Hspec

grid :: Grid
grid = createGrid (1.0, 1.0) 2.0 2.0 0.0 0.0

compressed :: CompressedGrid
compressed = [(0,[(0,2)]),(1,[(0,2)])]

spec :: Spec
spec =
  describe "compressGrid" $ do
    it "shoult compress grid" $
      compressGrid grid `shouldBe` compressed
