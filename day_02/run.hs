-- Usage: ghc --make run && ./run < input

import Data.List (group, intersect, sort)

checksum :: String -> String
checksum a = show $ f 2 a * f 3 a
  where
    f n = length
        . filter (not . null)
        . map (filter ((n ==) . length) . group . sort)
        . lines

common :: String -> String
common input =
  let ls = lines input
      diff (a, b) = a /= b
   in (\(a, b, _) -> intersect a b)
      . head
      . filter (\(_, _, a) -> length a == 1)
      . concatMap (\a -> map (\b -> (a, b, filter diff $ zip a b)) $ ls)
      $ ls

main = getContents >>= \a
  -> print ("checksum: " ++ checksum a)
  >> print ("common: " ++ common a)

