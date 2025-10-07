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

fixCols :: [(Int, [a])] -> [(Int, [a])]
fixCols pairs = zip new (map snd pairs)
  where (horig:torig) = map fst pairs
        lens = map (length . snd) pairs
        new = scanl1 (+) $ horig : zipWith (+) torig lens

getNums = fixCols . getPred isDigit
makeMap = Map.fromList . concat . zipWith (\r -> map (\ (c, v) -> ((r, c), v))) [0..]

getGears :: [String] -> [(Int, Int)]
getGears s =
  let gearlist = map (elemIndices '*') s
      gearpairs = zipWith (\ r c -> map (r,) c) [0..] gearlist
  in concat gearpairs 

type Coord = (Int, Int)
type ID = Int

expandNum :: Int -> (Coord, String) -> ([(Coord, ID)], (ID, String))
expandNum i ((r, c), val) = (coord2id, (i, val))
  where coord2id = map ((,i) . (r,)) [c..(c - 1 + length val)]

neighbors :: Coord -> [Coord]
neighbors (r, c) = [(r', c') | r' <- [r-1..r+1], c' <- [c-1..c+1], (r',c') /= (r,c)]

getProd :: (Int, Int) -> Map Coord ID -> Map ID String -> Int
getProd gear coord2id id2num
  | length nums == 2 = product nums
  | otherwise = 0
  where checks = neighbors gear
        findnum ij = Map.lookup ij id2num
        findid ij = Map.lookup ij coord2id
        numstrs = nub $ mapMaybe findnum $ mapMaybe findid checks
        nums = map (\x -> read x :: Int) numstrs

main :: IO ()
main = do
  input <- readFile "2023/03/input"
  let contents = lines input
      lnums = makeMap $ map getNums contents
      (coord2id_list, id2num_list) = unzip $ zipWith expandNum [0..] (Map.toList lnums)
      coord2id = Map.fromList $ concat coord2id_list
      id2num = Map.fromList id2num_list
      -- Don't need to make gears a Map.
      gears = getGears contents
      gprod = map (\g -> getProd g coord2id id2num) gears
      result = sum gprod
    in print result
