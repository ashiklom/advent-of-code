import System.IO

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Ord as Ord

import qualified Data.List as List

data Rank
  = Two | Three | Four | Five | Six
  | Seven | Eight | Nine | Ten | J 
  | Q | K | A 
  deriving (Show, Eq, Ord)

data Hand
  = HighCard [Rank]
  | OnePair [Rank]
  | TwoPair [Rank]
  | ThreeKind [Rank]
  | FullHouse [Rank]
  | FourKind [Rank]
  | FiveKind [Rank]
  deriving (Show, Eq, Ord)

mkHand :: [Rank] -> Hand
mkHand cards
  | grouplengths == [5] = FiveKind cards
  | grouplengths == [4,1] = FourKind cards
  | grouplengths == [3,2] = FullHouse cards
  | grouplengths == [3,1,1] = ThreeKind cards
  | grouplengths == [2,2,1] = TwoPair cards
  | grouplengths == [2,1,1,1] = OnePair cards
  | otherwise = HighCard cards
  where sortedcards = List.sort cards
        groups = List.group sortedcards
        grouplengths = List.sortBy (Ord.comparing Ord.Down) (map length groups)

mkRank :: Char -> Rank
mkRank s = case Map.lookup s c2r of
  (Just r) -> r
  Nothing -> error "Invalid character"
  where
      c2r = Map.fromList [
        ('2', Two), ('3', Three), ('4', Four), ('5', Five),
        ('6', Six), ('7', Seven), ('8', Eight), ('9', Nine),
        ('T', Ten), ('J', J), ('Q', Q), ('K', K), ('A', A)
        ]

readHand :: String -> Hand
readHand = mkHand . map mkRank

parseLine :: String -> (Hand, Int)
parseLine s = (readHand handstr, read score :: Int)
  where [handstr, score] = words s

calcScore :: Int -> (Hand, Int) -> Int
calcScore i (_, bet) = i * bet

main = do
  input <- readFile "2023/07/input"
  let contents = lines input
      pairs = map parseLine contents
      sortedpairs = List.sort pairs
      result = sum $ zipWith calcScore [1..] sortedpairs
    in print result
