{-| Grid of rows
-}
module GridBox.Grid
  (
    Grid
  , createGrid
  , makeGrid
  ) where

import GridBox.Row (Row, makeRow)

import Data.List (map)

-- | A grid of rows.
type Grid = [Row]

-- | Create a grid with given size width, height, x- and y-offset.
createGrid :: (Double, Double) -> Double -> Double -> Double -> Double -> Grid
createGrid (sizeX, sizeY) width height xOffset yOffset =
  makeGrid [0..(ceiling((width-1)/sizeX) :: Int)] [0..(ceiling((height-1)/sizeY) :: Int)] (sizeX, sizeY) xOffset yOffset

-- | Create a grid with given List of row- and col-indexes, size, x- and y-offset.
makeGrid :: [Int] -> [Int] -> (Double, Double) -> Double -> Double -> Grid
makeGrid rows cols size xOffset yOffset =
  map (makeRow size xOffset yOffset cols) rows

