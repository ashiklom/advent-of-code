import System.IO

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read (rational)
import Data.Either (rights)
import Data.Char (isDigit)

readNums :: Text -> [Float]
readNums s = map fst . rights $ map rational $ Text.words numpart
  where numpart = Text.dropWhile (not . isDigit) s

-- time = push + go
-- dist = push * go
--      = push * (time - push)
--      = -push^2 + push * time
-- push^2 - push*time + dist = 0
quadSolve :: (Float, Float) -> (Float, Float)
quadSolve (time, dist) = (minus, plus)
  where b4ac = sqrt(b^2 - 4*a*c)
        minus = (-b - b4ac) / (2 * a)
        plus = (-b + b4ac) / (2 * a)
        a = 1
        b = -time
        c = dist

nSolutions :: (Float, Float) -> Int
nSolutions (a, b) = length xs
  where xs = (takeWhile (<b) . dropWhile (<=a)) [0..]

main = do
  input <- readFile "2023/06/input"
  let [timestr, diststr] = Text.lines $ Text.pack input
      inputs = zip (readNums timestr) (readNums diststr)
      solutions = map (nSolutions . quadSolve) inputs
    in print (product solutions)
