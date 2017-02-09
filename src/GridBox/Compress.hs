{-| Grid compression
-}
module GridBox.Compress
  ( Length
  , CompressedGrid
  , CompressedRow
  , CompressedCols
  , CompressedCol
  , compressGrid
  , compressRow
  , compressBoxes
  , compressGroupedRows
  , compressGroupedRow
  , compressCols
  , fromCols
  , mergeCols
  ) where

import GridBox.Grid (Grid)
import GridBox.Row (Row)
import GridBox.Box (Box, BoxRow, BoxCol)
import GridBox.Grouping (GroupedRow, GroupedByRow, groupBoxesByRow)

-- | Length of a compressed col
type Length = Int

-- | Compressed grid
type CompressedGrid = [CompressedRow]

-- | Compressed row
type CompressedRow = (BoxRow, CompressedCols)

-- | Compressed cols
type CompressedCols = [CompressedCol]

-- | Compressed col
type CompressedCol = (BoxCol, Length)

-- | Compress grid
compressGrid :: Grid -> CompressedGrid
compressGrid = compressBoxes . concat

-- | Compress row
compressRow :: Row -> CompressedGrid
compressRow = compressBoxes

-- | Compress boxes
compressBoxes :: [Box] -> CompressedGrid
compressBoxes boxes =
  compressGroupedRows $ groupBoxesByRow boxes

-- | Compress grouped rows
compressGroupedRows :: GroupedByRow -> CompressedGrid
compressGroupedRows rows =
  map compressGroupedRow rows

-- | Compress grouped row
compressGroupedRow :: GroupedRow -> CompressedRow
compressGroupedRow (a, cols) =
  (a, compressCols cols)

-- | Compress cols
compressCols :: [BoxCol] -> CompressedCols
compressCols =
  mergeCols . fromCols

-- Convert list of box cols to compressed cols with length = 1
fromCols :: [BoxCol] -> CompressedCols
fromCols =
  map (\l -> (l, 1))

-- | Merge cols that have neighbours.
-- Each col a and b is merged to c. The index of c is the index of a and the
-- length of c is the sum of length a and b. 
mergeCols :: CompressedCols -> CompressedCols
mergeCols [] = []
mergeCols (a:[]) = [a]
mergeCols ((ac, al):(bc, bl):xs)
    | ac == bc - al = mergeCols $ (ac, al+bl):xs
    | otherwise = (ac, al) : (mergeCols $ (bc, bl):xs)

