import System.IO

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read (double)
import Data.Either (fromRight)
import Data.Char (isDigit)

readNums :: Text -> Double
readNums s = fst $ fromRight (0,Text.singleton ' ') $ double numstring
  where numpart = Text.dropWhile (not . isDigit) s
        numstring = Text.concat $ Text.words numpart

-- time = push + go
-- dist = push * go
--      = push * (time - push)
--      = -push^2 + push * time
-- push^2 - push*time + dist = 0
quadSolve :: (Double, Double) -> (Double, Double)
quadSolve (time, dist) = (minus, plus)
  where b4ac = sqrt(b^2 - 4*c)
        minus = (-b - b4ac) / 2
        plus = (-b + b4ac) / 2
        b = -time
        c = dist

simulate :: Int -> Int -> Int
simulate push time = push * (time - push)

getLower :: Double -> Int -> Int -> Int
getLower push time dist
  | simulate lower time > dist = lower
  | otherwise = upper
  where lower = ceiling push
        upper = lower + 1

getUpper :: Double -> Int -> Int -> Int
getUpper push time dist
  | simulate upper time > dist = upper
  | otherwise = lower
  where upper = ceiling push
        lower = upper - 1

nSolutions :: Int -> Int -> (Double, Double) -> Int
nSolutions time dist (a, b) = 1 + b' - a'
  where a' = getLower a time dist
        b' = getUpper b time dist

main = do
  input <- readFile "2023/06/input"
  let [timestr, diststr] = Text.lines $ Text.pack input
      inputs = (readNums timestr, readNums diststr)
      quads = quadSolve inputs
      solutions = nSolutions (floor $ fst inputs) (floor $ snd inputs) quads
    in print solutions
