import System.IO
import Data.IntMap.Strict (IntMap)
-- import qualified Data.Map as Map
import qualified Data.IntMap.Strict as Map

import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe

import qualified Data.Either as Either

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read

import Data.Foldable (foldl')

readNums :: Text -> [Int]
readNums s = map fst $ Either.rights $ map Text.Read.decimal (Text.words s)

expandRange :: [Int] -> IntMap Int
expandRange [dest,start,range] = Map.fromList $ take range (zip [start..] [dest..])

parseBlock :: Text -> IntMap Int
parseBlock block = nummap
  where (mtype:numlines) = Text.lines block
        numlist = map readNums numlines
        nummap = Map.unions $ map expandRange numlist

getMap :: IntMap Int -> [Int] -> [Int]
getMap nmap = map (\x -> Maybe.fromMaybe x (Map.lookup x nmap))

main = do
  input <- readFile "2023/05/input"
  let (seedblock:rawblocks) = Text.splitOn (Text.pack "\n\n") $ Text.pack input
      [_, seedstring] = Text.splitOn (Text.pack ":") seedblock
      seeds = readNums seedstring
      blocks = map parseBlock rawblocks
      result = foldl' (flip getMap) seeds blocks
    in print $ minimum result
