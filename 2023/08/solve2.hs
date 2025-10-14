import System.IO

import Data.Char (isSpace)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromJust)

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
  | last key == 'Z' = (i, key)
  | otherwise = step lrlist smap (i+1) (f val)
  where lr = lrlist !! i
        f = if lr == 'L' then fst else snd
        val = case Map.lookup key smap of
          Just x -> x
          Nothing -> error "Invalid key"

main = do
  input <- readFile "2023/08/input"
  let (steps, smap) = readInput input
      starts = filter ((=='A') . last) $ Map.keys smap
      dostep = step (cycle steps) smap
      results = map (dostep 0) starts
      -- Conveniently, the results just loop over the same steps,
      -- and do so a prime number of times. So, the answer is just the product 
      -- of the number of loops (minimum number to hit all loops) times the 
      -- length of each loop `(length steps)`.
      answer = (* length steps) $ product $ map ((`div` length steps) . fst) results
    in print answer
