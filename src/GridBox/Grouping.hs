{-| Grouping of boxes
-}
module GridBox.Grouping
  ( GroupedByRow
  , GroupedRow
  , groupBoxesByRow
  ) where

import GridBox.Box (Box, BoxRow, BoxCol, fromBoxes)

import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Ord (comparing)
import Control.Arrow ((&&&))

-- | Grouped box positions
type GroupedByRow = [GroupedRow]

-- | Grouped row
type GroupedRow = (BoxRow, [BoxCol])

-- | Group given list of boxes by row
groupBoxesByRow :: [Box] -> GroupedByRow
groupBoxesByRow =
  groupPairs . fromBoxes

-- | Group pairs
groupPairs :: Ord a => [(a, b)] -> [(a, [b])]
groupPairs =
  map ((fst . head) &&& map snd)
    . groupBy ((==) `on` fst)
    . sortBy (comparing fst)
