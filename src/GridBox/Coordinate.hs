{-| Coordinate
-}
module GridBox.Coordinate
    ( fromBox
    , fromBoxes
    , getX
    , getY
    , topX
    , topY
    , transform
    , transformOffset
    ) where

import GridBox.Box (Box(width, height, x, y))

-- | Create a List of (x,y) coordinates with given size.
fromBox :: Box -> [(Double, Double)]
fromBox box = [
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

-- | Convert all boxes in list from Box to (x, y) coordinates
fromBoxes :: [Box] -> [[(Double, Double)]]
fromBoxes boxes =
  map fromBox boxes

-- | Get the x-position for given row, size and x-offset.
getX :: Int -> Double -> Double -> Double
getX col size xOffset =
  topX (fromIntegral(col) * size + size/2) xOffset

-- | Get the y-position for given col, size and y-offset.
getY :: Int -> Double -> Double -> Double
getY row size yOffset =
  topY (fromIntegral(row) * size + size/2) yOffset

-- | Get the given position with x-offset.
topX :: Double -> Double -> Double
topX pos xOffset = 
  pos + xOffset

-- | Get the given position with y-offset.
topY :: Double -> Double -> Double
topY pos  yOffset =
  pos + yOffset

-- | Transform given coordinates with position (x, y) and scale.
transform :: (Double, Double) -> Double -> (Double, Double) -> (Double, Double)
transform =
  transformOffset (0,0)

-- | Transform given coordinates with position (x, y) scale, x- and y-offset.
transformOffset :: (Double, Double) -> (Double, Double) -> Double -> (Double, Double) -> (Double, Double)
transformOffset (xOffset, yOffset) (ox, oy) scale (x, y) =
  (topX (scale * x) (xOffset + ox), topY (scale *y) (yOffset + oy))
