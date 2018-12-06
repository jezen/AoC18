-- Usage: ghc --make run && ./run < input

import Data.Char
import Data.List

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq [x] = True
allEq xs = all (== head xs) (tail xs)

units :: Char -> Char -> Bool
units a b = toLower a == toLower b && a /= b

maxTwo :: String -> [String]
maxTwo [] = []
maxTwo [c1] = [[c1]]
maxTwo [c1,c2] = [[c1,c2]]
maxTwo (c1:c2:cs) = (c1:[c2]):[cs]

react :: String -> String
react = concat . filter allEq . concatMap maxTwo . groupBy units

polymer :: String -> String
polymer p = if p == react p then p else polymer (react p)

shortPoly :: String -> Int
shortPoly i = minimum . map f $ ['a'..'z']
  where f c = length . polymer . filter (\a -> toLower a /= toLower c) $ i

main :: IO ()
main = do
  input <- getContents
  print $ "Polymer length: " ++ show (length (polymer input))
  print $ "Shortest polymer length: " ++ show (shortPoly input)

