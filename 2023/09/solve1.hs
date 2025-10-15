import System.IO

parseLine :: String -> [Int]
parseLine line = [read x :: Int | x <- words line]

getToZero :: [[Int]] -> [[Int]]
getToZero xxx@(x:xs)
  | all (==0) x = xxx
  | otherwise = getToZero (diff:xxx)
  where diff = zipWith subtract (init x) (tail x)

expand :: [[Int]] -> [[Int]]
expand (x:y:xs) = expand (y':xs)
  where 
    x' = 0:x
    y' = zipWith (+) x' y
expand [x] = [x]

solve :: [Int] -> Int
solve x = (last . head . expand . getToZero) [x]

main = do
  input <- readFile "2023/09/input"
  let seqs = map parseLine $ lines input
      answers = map solve seqs
    in print (sum answers)

