import System.IO

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Maybe as Maybe

data Pipe = Vert | Horiz | NE | NW | SW | SE | Start | Ground deriving (Show, Eq)

type Coord = (Int, Int)
type PipeMap = Map Coord Pipe

getPipe :: Char -> Pipe
getPipe '.' = Ground
getPipe 'S' = Start
getPipe '|' = Vert
getPipe '-' = Horiz
getPipe 'L' = NE
getPipe 'J' = NW
getPipe 'F' = SE
getPipe '7' = SW

readLine :: Int -> String -> [(Coord, Pipe)]
readLine row s = zip (map (row,) [0..]) (map getPipe s)

readInput :: String -> PipeMap
readInput input = Map.fromList $ concat $ zipWith readLine [0..] (lines input)

data Direction = N | E | S | W deriving (Show, Eq)

move :: Coord -> Direction -> Coord
move (r,c) N = (r-1, c) 
move (r,c) S = (r+1, c) 
move (r,c) E = (r, c+1) 
move (r,c) W = (r, c-1) 

turn :: Pipe -> Direction -> Direction
turn Vert N = N
turn Vert S = S
turn Horiz E = E
turn Horiz W = W
turn NE S = E
turn NE W = N
turn SE N = E
turn SE W = S
turn NW S = W
turn NW E = N
turn SW N = W
turn SW E = S
turn p d = error $ "Invalid combination: Pipe " ++ show p ++ " Direction " ++ show d

data Position = Position {coord :: Coord, dir :: Direction, pipe :: Pipe} deriving Show

tryPipe :: PipeMap -> Coord -> Direction -> [Pipe] -> Maybe Position
tryPipe pmap coord dir pipes = do
  pipe <- Map.lookup coord pmap
  if pipe `elem` pipes then Just (Position {coord=coord, dir=dir, pipe=pipe} ) else Nothing

findStartPipes :: PipeMap -> Coord -> (Position, Position)
findStartPipes pmap (r, c) = case ab of
  [a, b] -> (a, b)
  _ -> error $ "Invalid result. Expected two results but found " ++ show (length ab)
  where
    up = ((r-1, c), N, [Vert, SW, SE])
    down = ((r+1, c), S, [Vert, NW, NE])
    left = ((r, c-1), W, [Horiz, NE, SE])
    right = ((r, c+1), E, [Horiz, NW, SW])
    ab = Maybe.mapMaybe (\(going,coord,pipes) -> tryPipe pmap going coord pipes) [up, down, left, right]

nextPipe :: PipeMap -> Position -> Position
nextPipe pmap x = Position {coord=newCoord, dir=newDir, pipe=pmap Map.! newCoord }
  where
    newDir = turn (pipe x) (dir x)
    newCoord = move (coord x) newDir

type History = [Coord]

navigatePipes :: PipeMap -> (History, History) -> (Position, Position) -> (History, History)
navigatePipes pmap (hist1, hist2) (p1, p2)
  | coord p1 `elem` hist2 = (hist1, hist2)
  | coord p2 `elem` hist1 = (hist1, hist2)
  | otherwise = navigatePipes pmap (hist1', hist2') (p1', p2')
    where
      p1' = nextPipe pmap p1
      p2' = nextPipe pmap p2
      hist1' = coord p1' : hist1
      hist2' = coord p2' : hist2

main = do
  input <- readFile "2023/10/input"
  let 
    pmap = readInput input
    start = head $ Map.keys $ Map.filter (==Start) pmap
    (a, b) = findStartPipes pmap start
    (x, y) = navigatePipes pmap ([], []) (a, b)
    result = max (1 + length x) (1 + length y)
    in print result
