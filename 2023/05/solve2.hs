import System.IO

import qualified Data.List as List

import qualified Data.Either as Either

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read

import Data.Foldable (foldl')

readNums :: Text -> [Int]
readNums s = map fst $ Either.rights $ map Text.Read.decimal (Text.words s)

type Pos = Int
type Shift = Int
type Pair = (Pos, Shift)
type Lookup = (Pos, Shift, Pos)

readSeeds :: Text -> [Pair]
readSeeds s = pair seedlist
  where seedlist = readNums s
        pair seedlist = case seedlist of
          [] -> []
          start:len:rest -> (start, len): pair rest

num2Range :: [Int] -> Lookup
num2Range [dest,start,range] = (start, range, dest)

parseBlock :: Text -> [Lookup]
parseBlock block = nummap
  where (mtype:numlines) = Text.lines block
        numlist = map readNums numlines
        nummap = List.sort $ map num2Range numlist

coalesce :: [Pair] -> [Pair]
coalesce [] = []
coalesce (x@(x0,dx):y@(y0,dy):rest)
  | x1p < y0 = x : coalesce (y:rest)
  | x1p == y0 = (x0, dx+dy) : coalesce rest
  | x1p >= y1p = coalesce (x:rest)
  | x1p < y1p = coalesce ((x0,1+y1p-x0):rest)
  where x1p = x0 + dx
        y1p = y0 + dy
coalesce [x] = [x]

placeSeed :: [Lookup] -> Pair -> [Pair]
placeSeed [] seed = [seed]
placeSeed ((a0, da, b0):xs) seed@(s0, ds)
  -- Completely outside range
  | s0 > a1 = placeSeed xs seed
  | (s0 < a0) && (s1 < a0) = placeSeed xs seed
  -- Completely inside range
  | (s0 >= a0) && (s1 <= a1) = [(b0+s0-a0, ds)]
  -- Partially inside range
  | (s0 < a0) && (s1 < a1) = [(s0, a0-s0), (b0, ds-(a0-s0))]
  | (s0 >= a0) && (s1 > a1) = (b0+s0-a0, 1+a1-s0) : placeSeed xs (1+a1, s1-a1)
  where s1 = s0 + ds - 1
        a1 = a0 + da - 1

placeSeeds :: [Lookup] -> [Pair] -> [Pair]
placeSeeds looks seeds = coalesce $ List.sort $ concatMap (placeSeed looks) seeds

main = do
  input <- readFile "2023/05/input"
  let (seedblock:rawblocks) = Text.splitOn (Text.pack "\n\n") $ Text.pack input
      [_, seedstring] = Text.splitOn (Text.pack ":") seedblock
      seeds = readSeeds seedstring
      blocks = map parseBlock rawblocks
      result = foldl' (flip placeSeeds) seeds blocks
    in print $ (fst . minimum) result
