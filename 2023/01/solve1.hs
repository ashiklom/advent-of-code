import System.IO

main = do
  contents <- readFile "2023/01/input"
  putStr $ show $ sum $ getNums contents
  putStr "\n"

onlyNumbers :: String -> String
onlyNumbers "" = ""
onlyNumbers (x:xs)
  | x `elem` ['1'..'9'] = x : onlyNumbers xs
  | otherwise           = onlyNumbers xs

parseLine :: String -> Int
parseLine input = 
  let s = onlyNumbers input
      a = head s
      z = last s
      out = read [a, z] :: Int
  in out

getNums :: String -> [Int]
getNums input = 
  let allLines = lines input
      nums = map parseLine allLines
  in nums
