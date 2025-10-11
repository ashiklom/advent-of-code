import System.IO

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read (decimal)
import Data.Either (fromRight)
import Data.Char (isDigit)

readNums :: Text -> Int
readNums s = fst $ fromRight (0,Text.singleton ' ') $ decimal numstring
  where numpart = Text.dropWhile (not . isDigit) s
        numstring = Text.concat $ Text.words numpart

-- time = push + go
-- dist = push * go
--      = push * (time - push)
--      = -push^2 + push * time
-- push^2 - push*time + dist = 0
quadSolve :: (Int, Int) -> (Double, Double)
quadSolve (time, dist) = (minus, plus)
  where b4ac = sqrt(b^2 - 4*c)
        minus = (-b - b4ac) / 2
        plus = (-b + b4ac) / 2
        b = fromIntegral (-time) :: Double
        c = fromIntegral dist :: Double

nSolutions :: (Double, Double) -> Int
nSolutions (a, b) = b' - a'
  where a' = floor a
        b' = floor b

main = do
  input <- readFile "2023/06/input"
  let [timestr, diststr] = Text.lines $ Text.pack input
      inputs = (readNums timestr, readNums diststr)
      quads = quadSolve inputs
      solutions = nSolutions quads
    in print solutions
