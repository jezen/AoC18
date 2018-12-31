-- Usage: ghc -O2 --make run && ./run < input

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Char                    (isDigit)
import           Data.List                    (intercalate)
import           Text.ParserCombinators.ReadP

newtype Position = Position (Int, Int) deriving (Show, Eq)

newtype Velocity = Velocity (Int, Int) deriving (Show, Eq)

newtype Second = Second Int deriving (Num, Show)

data Point = Point Position Velocity deriving (Show, Eq)

instance Ord Point where
  Point (Position (x1, _)) _ `compare` Point (Position (x2, _)) _ =
    x1 `compare` x2

num :: ReadP Char
num = satisfy (\c -> isDigit c || c == '-')

parsePoint :: ReadP Point
parsePoint = do
  string "position=<"
  skipSpaces
  pX <- read <$> many1 num
  satisfy (== ',')
  skipSpaces
  pY <- read <$> many1 num
  string "> velocity=<"
  skipSpaces
  vX <- read <$> many1 num
  satisfy (== ',')
  skipSpaces
  vY <- read <$> many1 num
  return $ Point (Position (pX, pY)) (Velocity (vX, vY))

toPoints :: String -> [Point]
toPoints = map (fst . last . readP_to_S parsePoint) . lines

pointsAtTime :: [Point] -> Second -> [Point]
pointsAtTime points (Second s) = map (f s) points
  where
    f s (Point (Position (pX, pY)) velocity@(Velocity (vX, vY))) =
      Point (Position (pX + s * vX, pY + s * vY)) velocity

positionsForPoints :: [Point] -> [(Int, Int)]
positionsForPoints = map (\(Point (Position p) _) -> p)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l  = take n l : chunksOf n (drop n l)

showPoints :: [Point] -> String
showPoints ps =
  let xs   = map fst $ positionsForPoints ps
      ys   = map snd $ positionsForPoints ps
      s p  = if p `elem` positionsForPoints ps then '▓' else '░'
      grid = [ s (x, y) | y <- [minimum ys .. maximum ys]
                        , x <- [minimum xs .. maximum xs] ]
   in unlines . chunksOf (maximum xs - minimum xs + 1) $ grid

canvasWidth :: [Point] -> Int
canvasWidth ps = maximum xs - minimum xs + 1
  where xs = map fst $ positionsForPoints ps

minimumCanvas :: [Point] -> Second -> Second
minimumCanvas ps s
  | this < next = s
  | otherwise   = minimumCanvas ps (s + 1)
  where this = canvasWidth $ pointsAtTime ps s
        next = canvasWidth $ pointsAtTime ps (s + 1)

main :: IO ()
main = do
  points <- toPoints <$> getContents
  let seconds@(Second s) = minimumCanvas points (Second 0)
  print "Message"
  print "======="
  putStrLn $ showPoints $ pointsAtTime points seconds
  print $ "Seconds until message appears: " <> show s

