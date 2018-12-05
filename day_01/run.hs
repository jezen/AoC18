-- Usage: ghc --make run && ./run < input

import Data.Char (isDigit)
import Data.Set  (Set, empty, insert, member, size)

parse :: String -> [Int]
parse = map (read . filter (\c -> isDigit c || c == '-')) . lines

reachTwice :: Set Int -> Int -> [Int] -> Int
reachTwice seen acc (x:xs)
  | res `member` seen = res
  | otherwise = reachTwice (insert res seen) res xs
  where res = acc + x

main :: IO ()
main = do
  input <- parse <$> getContents
  print $ "total: " ++ show (sum input)
  print $ "reachTwice: " ++ show (reachTwice empty 0 (cycle input))

