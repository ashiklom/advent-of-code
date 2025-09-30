import System.IO
import Data.Char (isDigit)

main :: IO ()
main = do
  contents <- readFile "2023/01/input"
  print $ sum $ getNums contents

onlyNumbers :: String -> String
onlyNumbers = filter isDigit

parseLine :: String -> Int
parseLine input = 
  let s = onlyNumbers input
      out = read [head s, last s] :: Int
  in out

getNums :: String -> [Int]
getNums = map parseLine . lines
