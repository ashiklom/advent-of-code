import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Char (isSpace)

noblanks :: String -> String
noblanks = reverse . dropWhile isSpace . reverse . dropWhile isSpace

splits :: String -> String -> [String]
splits splitter str = case dropWhile (`elem` splitter) str of
  "" -> []
  s' -> noblanks a: splits splitter s''
    where (a, s'') = break (`elem` splitter) s'

type Color = String
type Count = Int

parseGame :: String -> Map Color Count
parseGame g = Map.fromListWith max (map parseDraw gsplit)
  where gsplit = splits ",;" g
        parseDraw s = (noblanks color, read n :: Int)
          where (n, color) = break isSpace s

parseLine :: String -> (Int, Map Color Count)
parseLine line = (index, gmap)
  where [indexpart, gamepart] = splits ":" line
        index = case stripPrefix "Game " indexpart of
          Just x -> read x :: Int
          Nothing -> error "Game index not found"
        gmap = parseGame gamepart

validGame :: Map Color Count -> Bool
validGame game = red && green && blue
  where getCount color count = case Map.lookup color game of
          Just n -> n <= count
          Nothing -> False
        red = getCount "red" 12
        green = getCount "green" 13
        blue = getCount "blue" 14

main :: IO ()
main = do
  input <- readFile "2023/02/input"
  let games = map parseLine $ lines input
      result = sum $ map (\ (id, game) -> if validGame game then id else 0) games
    in print result

