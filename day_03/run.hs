-- Usage: ghc --make run && ./run < input

import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Set (fromList, isSubsetOf)

newtype ClaimId = ClaimId Int deriving Show
newtype Offset = Offset (Int, Int) deriving Show
newtype Dimensions = Dimensions (Int, Int) deriving Show

data Claim = Claim
  { claimId         :: ClaimId
  , claimOffset     :: Offset
  , claimDimensions :: Dimensions
  } deriving Show

data Fabric = Fabric
  { fabricId :: ClaimId
  , fabricGrid :: [(Int, Int)]
  }

digit :: ReadP Char
digit = satisfy (\c -> c >= '0' && c <= '9')

parseClaimId :: ReadP ClaimId
parseClaimId = do
  string "#"
  id <- read <$> many1 digit
  return $ ClaimId id

parseOffset :: ReadP Offset
parseOffset = do
  satisfy (== ' ')
  satisfy (== '@')
  satisfy (== ' ')
  x <- read <$> many1 digit
  satisfy (== ',')
  y <- read <$> many1 digit
  return $ Offset (x, y)

parseDimensions :: ReadP Dimensions
parseDimensions = do
  satisfy (== ':')
  satisfy (== ' ')
  x <- read <$> many1 digit
  satisfy (== 'x')
  y <- read <$> many1 digit
  return $ Dimensions (x, y)

parseClaim :: ReadP Claim
parseClaim = do
  id         <- parseClaimId
  offset     <- parseOffset
  dimensions <- parseDimensions
  return $ Claim id offset dimensions

toClaims :: String -> [Claim]
toClaims = map (fst . head . reverse . readP_to_S parseClaim) . lines

-- List comprehensions are pretty badass ಠ_ರೃ
mkFabric :: Claim -> Fabric
mkFabric (Claim id (Offset (oX, oY)) (Dimensions (dX, dY))) =
  Fabric id [ (x, y) | x <- [oX..(oX + dX - 1)], y <- [oY..(oY + dY - 1)] ]

groupCoords :: [Claim] -> [[(Int, Int)]]
groupCoords = group . sort . concatMap (fabricGrid . mkFabric)

overlaps :: [Claim] -> Int
overlaps = length . filter ((> 1) . length) . groupCoords

noCollide :: [Claim] -> ClaimId
noCollide claims =
  let freeCoords = concat . filter ((== 1) . length) . groupCoords $ claims
   in fabricId
        . head
        . filter (\(Fabric _ g) -> fromList g `isSubsetOf` fromList freeCoords)
        . map mkFabric
        $ claims

main = do
  claims <- toClaims <$> getContents
  print $ "overlaps: " ++ show (overlaps claims)
  print $ "noCollide: " ++ show (noCollide claims)

