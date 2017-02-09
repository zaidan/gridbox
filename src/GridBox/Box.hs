{-| Box in a row
-}
module GridBox.Box
  ( Box(..)
  , BoxRow
  , BoxCol
  , BoxPosition
  , BoxPositions
  , makeBox
  , makeVectors
  , fromBox
  , fromBoxes
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

-- | Box position
type BoxPosition = (BoxRow, BoxCol)

-- | Box positions
type BoxPositions = [BoxPosition]

-- | Create a box with position (x,y) at row and col with given size.
makeBox :: Double -> Double -> Int -> Int -> (Double, Double) -> Box
makeBox x y row col (width, height) =
  Box x y row col width height

-- | Create a List of (x,y) with given size.
makeVectors :: Box -> [(Double, Double)]
makeVectors box = [
  (boxX + offX, boxY + offY),
  (boxX + offX, boxY - offY),
  (boxX - offX, boxY - offY),
  (boxX - offX, boxY + offY),
  (boxX + offX, boxY + offY)
  ]
  where
    offX = (width box) / 2
    offY = (height box) / 2
    boxX = x box
    boxY = y box

-- | Convert from Box to (row, col)
fromBox :: Box -> BoxPosition
fromBox box =
  (row box, col box)

-- | Convert all boxes in list from Box to (row, col)
fromBoxes :: [Box] -> BoxPositions
fromBoxes boxes =
  map fromBox boxes

