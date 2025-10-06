import System.IO
import Data.Char (isDigit)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

getPred :: (Char -> Bool) -> String -> [(Int, String)]
getPred _ "" = []
getPred pred s = case findIndex pred s of
  Just n -> let (nums, rest) = span pred (drop n s)
            in (n, nums) : getPred pred rest
  Nothing -> []

fixCols :: [(Int, String)] -> [(Int, String)]
fixCols pairs = zip new (map snd pairs)
  where (horig:torig) = map fst pairs
        lens = map (\ (_,num) -> length num) pairs
        new = scanl1 (+) $ horig : zipWith (+) torig lens

getNums = fixCols . getPred isDigit
getGears = fixCols . getPred (== '*')
makeMap = Map.fromList . concat . zipWith (\ r stuff -> map (\ (c, v) -> ((r, c), v)) stuff) [0..]

-- Expand nums map to two maps:
-- First is id -> num
-- Second is idxs -> id
expandNum :: Map (Int, Int) String -> (Map (Int, Int) Int, Map Int String)
expandNum nmap = 
  let idmap = Map.fromList $ zip [0..] (Map.elems nmap)
      getPos (r,c) v = map (,v) (map (r,) [c..(c+(length v)-1)])
      posmap = Map.fromList $ concat $ Map.mapWithKey getPos nmap
      result = zip [0..] ()
      (poslist, idlist) = unzip result
  in (posmap, idmap)

getProd :: ((Int, Int), a) -> Map (Int, Int) String -> Int
getProd ((r, c), _) nummap
  | length nums == 2 = foldr1 (*) nums
  | otherwise = 0
  where up = r-1
        down = r+1
        left = c-1
        right = c+1
        ups = map (up,) [left..right]
        downs = map (down,) [left..right]
        row = [(r,left), (r,right)]
        checks = concat [ups, downs, row]
        numstrs = mapMaybe (`Map.lookup` nummap) checks
        nums = map (\x -> read x :: Int) numstrs

main :: IO ()
main = do
  input <- readFile "2023/03/testinput"
  let contents = lines input
      lnums = makeMap $ map getNums contents
      (num_id, num_pos) = expandNum lnums
      gears = makeMap $ map getGears contents
      gprod = map (`getProd` lnums) (Map.toList gears)
      result = sum gprod
    in print (num_id, num_pos)
