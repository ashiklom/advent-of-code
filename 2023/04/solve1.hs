import System.IO
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import qualified Data.Either as Either

line2Set :: Text -> Set Int
line2Set line = Set.intersection winning mine
  where dropgame s = last $ Text.splitOn (Text.singleton ':') s
        leftright_str = Text.splitOn (Text.singleton '|') $ dropgame line
        makeSet = Set.fromList . map fst . Either.rights . map Text.Read.decimal . Text.words . Text.strip
        [winning, mine] = map makeSet leftright_str

calcScore :: Set Int -> Int
calcScore s
  | n > 0 = 2^(n-1)
  | otherwise = 0
  where n = length s

main :: IO ()
main = do
  input <- readFile "2023/04/input"
  let contents = Text.lines $ Text.pack input
      sets = map line2Set contents
      scores = map calcScore sets
    in print (sum scores)
