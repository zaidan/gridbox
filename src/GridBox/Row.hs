{-| Row of a grid
-}
module GridBox.Row
    (
      Row
    , getBoxPositionX
    , getBoxPositionY
    , createBox
    , makeRow
    , topX
    , topY) where

import GridBox.Box (Box, makeBox)
import Data.List (map)

-- | A row of boxes.
type Row = [Box]

-- | Get the x-position for given row, size and x-offset.
getBoxPositionX :: Int -> Double -> Double -> Double
getBoxPositionX col size xOffset =
  topX (fromIntegral(col) * size + size/2) xOffset

-- | Get the y-position for given col, size and y-offset.
getBoxPositionY :: Int -> Double -> Double -> Double
getBoxPositionY row size yOffset =
  topY (fromIntegral(row) * size + size/2) yOffset

-- | Create a box with given row, col, size, x- and y-offset.
createBox :: (Double, Double) -> Double -> Double -> Int -> Int -> Box
createBox (sizeX, sizeY) xOffset yOffset row col =
  makeBox (getBoxPositionX col sizeX xOffset) (getBoxPositionY row sizeY yOffset) row col (sizeX, sizeY)

-- | Create a row with given row, List of col-indexes, size, x- and y-offset.
makeRow :: (Double, Double) -> Double -> Double -> [Int] -> Int -> Row
makeRow size xOffset yOffset cols row =
  map (createBox size xOffset yOffset row) cols

-- | Get the given position with x-offset.
topX :: Double -> Double -> Double
topX pos xOffset = 
  pos + xOffset

-- | Get the given position with y-offset.
topY :: Double -> Double -> Double
topY pos  yOffset =
  pos + yOffset
