import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
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

power :: Map Color Count -> Int
power game = red * green * blue
  where getCount color = Maybe.fromMaybe 0 (Map.lookup color game)
        red = getCount "red"
        green = getCount "green"
        blue = getCount "blue"

main :: IO ()
main = do
  input <- readFile "2023/02/input"
  let games = map parseLine $ lines input
      result = sum $ map (\ (_, game) -> power game) games
    in print result

