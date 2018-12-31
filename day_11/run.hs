-- Usage: ghc -O2 --make run && ./run

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.List  (sort)
import           Data.Maybe (mapMaybe)

newtype Cell = Cell (Int, Int) deriving (Show, Eq)

newtype PowerLevel = PowerLevel Int deriving (Show, Num, Ord, Eq)

newtype SerialNumber = SerialNumber Int

newtype Size = Size Int deriving (Show, Eq)

data Quadrant = Quadrant Cell PowerLevel Size
  deriving (Show, Eq)

instance Ord Quadrant where
  Quadrant _ p1 _ `compare` Quadrant _ p2 _ = p1 `compare` p2

grid :: [Cell]
grid = [ Cell (x, y) | y <- [1..300], x <- [1..300] ]

hundredsDigit :: Int -> Int
hundredsDigit n = n `div` 100 `rem` 10

powerLevel :: SerialNumber -> Cell -> PowerLevel
powerLevel (SerialNumber s) (Cell (x, y)) =
  let rackId = x + 10
      power  = (rackId * y + s) * rackId
   in PowerLevel $ hundredsDigit power - 5

powerSquare :: SerialNumber -> Size -> Cell -> Maybe Quadrant
powerSquare serial size@(Size s) cell@(Cell (x1, y1)) =
  let x2 = x1 + s - 1
      y2 = y1 + s - 1
      square = [ Cell (x, y) | x <- [x1..x2], y <- [y1..y2] ]
      power  = sum . map (powerLevel serial) $ square
   in if any (> 300) [x2, y2] then Nothing else Just $ Quadrant cell power size

largestSquare :: SerialNumber -> [Size] -> Quadrant
largestSquare serial sizes = maximum quadrants
  where quadrantsForSize s = mapMaybe (powerSquare serial s) $ grid
        quadrants          = concatMap quadrantsForSize sizes

main :: IO ()
main = do
  let s = SerialNumber 8561
  print $ "Largest 3x3 square: " ++ show (largestSquare s [Size 3])
  -- Forget about the second part running in any reasonable length of time. To
  -- find my answer, I ran largestSquare on the first 30 Size values instead of
  -- all 300.
  -- From my research, it seems for this to run quickly I'd have to use a
  -- Summed-Area Table.
  print $ "Largest square: " ++ show (largestSquare s (map Size [1..300]))

