import System.IO
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import qualified Data.Either as Either
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

getCards :: Int -> Text -> [Int]
getCards i line = map (i+) [1..length isect]
  where dropgame s = last $ Text.splitOn (Text.singleton ':') s
        leftright_str = Text.splitOn (Text.singleton '|') $ dropgame line
        makeSet = Set.fromList . map fst . Either.rights . map Text.Read.decimal . Text.words . Text.strip
        [winning, mine] = map makeSet leftright_str
        isect = Set.intersection winning mine

calcNCards :: Map Int [Int] -> [Int] -> Int
calcNCards _ [] = 0
calcNCards nmap nums = total
  where total = length nums + sum counts
        counts = map (calcNCards nmap) (Maybe.mapMaybe (`Map.lookup` nmap) nums)

main :: IO ()
main = do
  input <- readFile "2023/04/input"
  let contents = Text.lines $ Text.pack input
      cardset = Map.fromList $ zip [1..] $ zipWith getCards [1..] contents
      counts = Map.map (calcNCards cardset) cardset
      result = sum (Map.elems counts) + length cardset
    in print result
