{-| Row of a grid
-}
module GridBox.Row
    ( Row
    , createBox
    , makeRow
    ) where

import GridBox.Box (Box, makeBox)
import GridBox.Coordinate (getX, getY)

import Data.List (map)

-- | A row of boxes.
type Row = [Box]

-- | Create a box with given row, col, size, x- and y-offset.
createBox :: (Double, Double) -> Double -> Double -> Int -> Int -> Box
createBox (sizeX, sizeY) xOffset yOffset row col =
  makeBox (getX col sizeX xOffset) (getY row sizeY yOffset) row col (sizeX, sizeY)

-- | Create a row with given row, List of col-indexes, size, x- and y-offset.
makeRow :: (Double, Double) -> Double -> Double -> [Int] -> Int -> Row
makeRow size xOffset yOffset cols row =
  map (createBox size xOffset yOffset row) cols
