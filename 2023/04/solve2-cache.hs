-- Trying to implement a cache, but it's way slower...

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import qualified Data.Either as Either
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Ord (comparing)
import qualified Data.Ord as Data.List

getCards :: Int -> Text -> [Int]
getCards i line = map (i+) [1..length isect]
  where dropgame s = last $ Text.splitOn (Text.singleton ':') s
        leftright_str = Text.splitOn (Text.singleton '|') $ dropgame line
        makeSet = Set.fromList . map fst . Either.rights . map Text.Read.decimal . Text.words . Text.strip
        [winning, mine] = map makeSet leftright_str
        isect = Set.intersection winning mine

type Cache = Map Int Int

calcNCards :: Cache -> Map Int [Int] -> Int -> (Int, Cache)
calcNCards cache nmap num = case Map.lookup num cache of
  Just count -> (count, cache)
  Nothing -> (total, newcache)
    where total = 1 + sum counts
          newcache = Map.insert num total cache
          numlist = Maybe.fromMaybe [] (Map.lookup num nmap)
          (counts, _) = unzip $ map (calcNCards cache nmap) numlist

main :: IO ()
main = do
  input <- readFile "2023/04/input"
  let contents = Text.lines $ Text.pack input
      cardmap = Map.fromList $ zip [1..] $ zipWith getCards [1..] contents
      cardlist = List.sortBy (comparing Data.List.Down) $ concat $ Map.elems cardmap
      (counts, _) = unzip $ map (calcNCards Map.empty cardmap) cardlist
      result = sum counts + length cardmap
    in print result
