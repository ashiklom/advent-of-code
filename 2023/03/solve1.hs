import System.IO
import Data.Char (isDigit)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

getPred :: (Char -> Bool) -> String -> [(Int, String)]
getPred _ "" = []
getPred pred s = case findIndex pred s of
  Just n -> let (nums, rest) = span pred (drop n s)
            in (n, nums) : getPred pred rest
  Nothing -> []

fixCols :: [(Int, String)] -> [(Int, String)]
fixCols pairs = zip new (map snd pairs)
  where iorig = map fst pairs
        lens = map (\ (_,num) -> length num) pairs
        new = scanl1 (+) $ head iorig : zipWith (+) (tail iorig) lens

getNums = fixCols . getPred isDigit
isNotNumDot x = not (isDigit x) && (x /= '.')
getSymbols = fixCols . getPred isNotNumDot
makeMap = Map.fromList . concat . zipWith (\ r stuff -> map (\ (c, v) -> ((r, c), v)) stuff) [0..]

getValue :: ((Int,Int), String) -> Map (Int,Int) a -> Int
getValue ((r, c), nums) symmap
  | touchesSym ((r, c), nums) = read nums :: Int
  | otherwise = 0
  where up = r - 1
        down = r + 1
        left = c - 1
        right = c + length nums
        ups = map (up,) [left..right]
        -- ups = zip (repeat up) [left..right]
        downs = map (down,) [left..right]
        -- downs = zip (repeat down) [left..right]
        row = [(r, left), (r, right)]
        checks = concat [ups, downs, row]
        touchesSym ((r, c), nums) = any (`Map.member` symmap) checks

main :: IO ()
main = do
  input <- readFile "2023/03/input"
  let contents = lines input
      lnums = makeMap $ map getNums contents
      lsyms = makeMap $ map getSymbols contents
      result = sum $ map (`getValue` lsyms) (Map.toList lnums)
    in print result
