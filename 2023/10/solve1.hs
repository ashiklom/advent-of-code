import System.IO

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Maybe as Maybe

data Pipe = Vert | Horiz | NE | NW | SW | SE | Start | Ground deriving (Show, Eq)

type Coord = (Int, Int)
type PipeMap = Map Coord Pipe

getPipe :: Char -> Pipe
getPipe c = case Map.lookup c c2r of
  (Just r) -> r
  Nothing -> error "Invalid character"
  where
      c2r = Map.fromList [
         ('|', Vert), ('-', Horiz), ('L', NE), ('J', NW)
        ,('7', SW), ('F', SE), ('.', Ground), ('S', Start)
        ]

readLine :: Int -> String -> [(Coord, Pipe)]
readLine row s = zip (map (row,) [0..]) (map getPipe s)

readInput :: String -> PipeMap
readInput input = Map.fromList $ concat $ zipWith readLine [0..] (lines input)

data Direction = N | E | S | W deriving (Show, Eq)

type Triple = (Coord, Direction, Pipe)

tryPipe :: PipeMap -> Coord -> Direction -> [Pipe] -> Maybe Triple
tryPipe pmap coord dir pipes = do
  pipe <- Map.lookup coord pmap
  if pipe `elem` pipes then Just (coord, dir, pipe) else Nothing

findStartPipes :: PipeMap -> Coord -> (Triple, Triple)
findStartPipes pmap (r, c) = (a, b)
  where
    up = ((r-1, c), N, [Vert, SW, SE])
    down = ((r+1, c), S, [Vert, NW, NE])
    left = ((r, c-1), W, [Horiz, NE, SE])
    right = ((r, c+1), E, [Horiz, NW, SW])
    [a, b] = Maybe.mapMaybe (\(going,coord,pipes) -> tryPipe pmap going coord pipes) [up, down, left, right]

going :: PipeMap -> Coord -> Direction -> (Coord, Direction, Pipe)
going pmap (r,c) N = ((r-1, c), N, pmap Map.! (r-1, c))
going pmap (r,c) E = ((r, c-1), E, pmap Map.! (r, c-1))
going pmap (r,c) S = ((r+1, c), S, pmap Map.! (r+1, c))
going pmap (r,c) W = ((r, c+1), W, pmap Map.! (r, c+1))

nextPipe :: PipeMap -> Coord -> Direction -> Pipe -> (Coord, Direction, Pipe)
nextPipe pmap coord dir pipe = going pmap coord newDir
  where
    newDir = case (dir, pipe) of
      (N, Vert) -> N
      (N, NW) -> W
      (N, NE) -> E
      (S, Vert) -> S
      (S, SW) -> W
      (S, SE) -> E
      (E, Horiz) -> E
      (E, NE) -> N
      (E, SE) -> S
      (W, Horiz) -> W
      (W, NW) -> N
      (W, SE) -> S

nextPipe' :: PipeMap -> (Coord, Direction, Pipe) -> (Coord, Direction, Pipe)
nextPipe' pmap (dir, coord, pipe) = nextPipe pmap dir coord pipe

-- nextPipe' pmap a

main = do
  input <- readFile "2023/10/testinput1"
  let pmap = readInput input
      (r0,c0) = head $ Map.keys $ Map.filter (==Start) pmap
    in print pmap
