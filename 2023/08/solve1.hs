import System.IO

import Data.Char (isSpace)

import Data.Map (Map)
import qualified Data.Map as Map

readSmap :: String -> (String, (String, String))
readSmap s = (keystr, (start, end))
  where (keystr, rest) = break isSpace s
        (start, end') = break (==',') $ filter (not . (`elem` " =()")) rest
        end = dropWhile (==',') end'

readInput :: String -> (String, Map String (String, String))
readInput input = (stepstr, smap)
  where stepstr:_:mapstr = lines input
        smap = Map.fromList $ map readSmap mapstr

step :: [Char] -> Map String (String, String) -> Int -> String -> (Int, String)
step lrlist smap i key
  | key == "ZZZ" = (i, "Done!")
  | otherwise = step lrlist smap (i+1) (f val)
  where lr = lrlist !! i
        f = if lr == 'L' then fst else snd
        val = case Map.lookup key smap of
          Just x -> x
          Nothing -> error "Invalid key"

main = do
  input <- readFile "2023/08/input"
  let (steps, smap) = readInput input
      result = step (cycle steps) smap 0 "AAA"
    in print (fst result)
