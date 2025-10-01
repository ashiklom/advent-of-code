import Data.Char (isDigit)
import Data.List (isPrefixOf)
import System.IO

main :: IO ()
main = do
  contents <- readFile "2023/01/input"
  print $ sum $ getNums contents

onlyNumbers :: String -> String
onlyNumbers "" = ""
onlyNumbers all@(x : xs)
  | isDigit x = x : onlyNumbers xs
  | "one" `isPrefixOf` all = '1' : onlyNumbers xs
  | "two" `isPrefixOf` all = '2' : onlyNumbers xs
  | "three" `isPrefixOf` all = '3' : onlyNumbers xs
  | "four" `isPrefixOf` all = '4' : onlyNumbers xs
  | "five" `isPrefixOf` all = '5' : onlyNumbers xs
  | "six" `isPrefixOf` all = '6' : onlyNumbers xs
  | "seven" `isPrefixOf` all = '7' : onlyNumbers xs
  | "eight" `isPrefixOf` all = '8' : onlyNumbers xs
  | "nine" `isPrefixOf` all = '9' : onlyNumbers xs
  | otherwise = onlyNumbers xs

parseLine :: String -> Int
parseLine input =
  let s = onlyNumbers input
      out = read [head s, last s] :: Int
   in out

getNums :: String -> [Int]
getNums = map parseLine . lines
