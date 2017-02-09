{-| Box in a row
-}
module GridBox.Box
  ( Box(..)
  , BoxRow
  , BoxCol
  , makeBox
  ) where

-- | A grid box with position (x,y) at row and col with size.  
data Box = Box
  { x :: Double
  , y :: Double
  , row :: BoxRow
  , col :: BoxCol
  , width :: Double
  , height :: Double
  }
  deriving (Read, Show, Eq)

-- | Row of a box
type BoxRow = Int

-- | Col of a box
type BoxCol = Int

-- | Create a box with position (x,y) at row and col with given size.
makeBox :: Double -> Double -> Int -> Int -> (Double, Double) -> Box
makeBox x y row col (width, height) =
  Box x y row col width height
