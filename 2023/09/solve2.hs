import System.IO

parseLine :: String -> [Int]
parseLine line = [read x :: Int | x <- words line]

getToZero :: [[Int]] -> [[Int]]
getToZero xxx@(x:xs)
  | all (==0) x = xxx
  | otherwise = getToZero (diff:xxx)
  where diff = zipWith subtract (init x) (tail x)

expand :: [Int] -> [[Int]] -> [[Int]]
expand x [] = [x]
expand x (y:ys) = expand y' ys
  where y' = zipWith (+) (0:x) y

solve :: [Int] -> Int
solve x = last $ head $ expand y ys
  where (y:ys) = getToZero [x]

main = do
  input <- readFile "2023/09/input"
  let seqs = map parseLine $ lines input
      answers = map (solve . reverse) seqs
    in print (sum answers)

