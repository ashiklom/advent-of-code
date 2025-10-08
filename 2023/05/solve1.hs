import System.IO
import Data.Array (Array, Ix)
import qualified Data.Array as Array

import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe

import qualified Data.Either as Either

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read

import Data.Foldable (foldl')

readNums :: Text -> [Int]
readNums s = map fst $ Either.rights $ map Text.Read.decimal (Text.words s)

type Lookup = (Int -> Maybe Int)

num2Range :: [Int] -> Lookup
num2Range [dest,start,range] = fun
  where fun i
          | Array.inRange startrange i = Just (dest + Array.index startrange i)
          | otherwise = Nothing
          where startrange = (start, start+range-1)

parseBlock :: Text -> [Lookup]
parseBlock block = nummap
  where (mtype:numlines) = Text.lines block
        numlist = map readNums numlines
        nummap = map num2Range numlist

getValue :: [Lookup] -> Int -> Int
getValue funs i = case dropWhile Maybe.isNothing (map ($ i) funs) of
  ((Just x):_) -> x
  [] -> i

getValues :: [Lookup] -> [Int] -> [Int]
getValues funs = map (getValue funs)

main = do
  input <- readFile "2023/05/input"
  let (seedblock:rawblocks) = Text.splitOn (Text.pack "\n\n") $ Text.pack input
      [_, seedstring] = Text.splitOn (Text.pack ":") seedblock
      seeds = readNums seedstring
      blocks = map parseBlock rawblocks
      result = foldl' (flip getValues) seeds blocks
    in print $ minimum result
