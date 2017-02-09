{-| Position
-}
module GridBox.Position
    ( BoxPosition
    , BoxPositions
    , fromBox
    , fromBoxes
    ) where

import GridBox.Box (Box(row, col), BoxRow, BoxCol)

-- | Box position
type BoxPosition = (BoxRow, BoxCol)

-- | Box positions
type BoxPositions = [BoxPosition]

-- | Convert from Box to (row, col)
fromBox :: Box -> BoxPosition
fromBox box =
  (row box, col box)

-- | Convert all boxes in list from Box to (row, col)
fromBoxes :: [Box] -> BoxPositions
fromBoxes boxes =
  map fromBox boxes
