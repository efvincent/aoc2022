{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2023.Day10 (sln2310) where

import Data.List (find)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Map.Strict as M (Map, toList, (!), insert, empty, member, (!))
import qualified Data.Set as S (Set, fromList, member, insert, union)
import Coord ( Coord(..), west, east, south, neighbors, north )
import Util (getSample, getPuzzle)
import System.Console.ANSI (SGR(SetColor), setSGR, ConsoleLayer (Foreground), ColorIntensity (Vivid, Dull), Color (Green, White, Red))

data Tile = NS | EW | NE | NW | SE | SW | G | Start | NonPath deriving (Show, Eq, Ord)
data Compass = N | S | E | W deriving (Show)
data Dir = Up | Down | Neither  deriving (Show)
data LoopDir = CW | CCW deriving (Show, Eq)
type Board  = M.Map Coord Tile
type Puzzle = (Board, Coord)

{-- Solutions----------------------------------------------------}

sln2310 :: String -> (Int,Int)
sln2310 s =
  let puz = parse s in
  (solve1 puz, solve2 puz)

solve1 :: Puzzle -> Int
solve1 puz =
  let [path1,path2] = mkPaths puz in
  loop 1 path1 path2
  where
    loop n (h1:p1) (h2:p2)
      | h1 == h2 = n
      | otherwise = loop (n+1) p1 p2
    loop _ _ _ = undefined

solve2 :: Puzzle -> Int
solve2 puz@(board,start) = loop 0 0
  where
    pathSet = 
      let [p1,p2] = mkPaths puz in
      S.insert start $ S.fromList p1 `S.union` S.fromList p2 
    loop count y =
      if C y 0 `M.member` board 
      then loop (countLine puz pathSet False y 0 count) (y+1)
      else count

countLine :: Puzzle -> S.Set Coord -> Bool -> Int -> Int -> Int -> Int
countLine puz@(board,_) pathSet contained y x count
  | not (M.member (C y x) board) = count
  | otherwise =
    let cur = C y x in
    if S.member cur pathSet then
      -- on the boundary 
      if isFlip cur then
        -- On the boundary and needs a flip. Flip but don't count
        countLine puz pathSet (not contained) y (x+1) count
      else
        -- On the boundary, but non flipper. don't count, don't flip
        countLine puz pathSet contained y (x+1) count
    else
      -- Not on the boundary. If inPath, then count
      let count' = if contained then count + 1 else count in
      countLine puz pathSet contained y (x+1) count'
  where
    flipTiles = S.fromList [NS,NE,NW]
    isFlip cur =
      case board M.! cur of
        t | t `S.member` flipTiles || (t == Start && S.member (cur + north) pathSet) -> True
        _ -> False

{-- Parsing ----------------------------------------------------}

parse :: String -> Puzzle
parse s =
  let ylines = zip [0..] . lines $ s :: [(Int, String)] in
  let board = foldl (\b (y,line) -> parseLine y b line) M.empty ylines in
  (board, (fst . fromJust . find ((== Start) . snd) . M.toList) board)

parseLine :: Int -> Board -> String -> Board
parseLine y board =
  foldl (\b (x,t) -> M.insert (C y x) t b) board . zip [0..] . map mkTile

mkTile :: Char -> Tile
mkTile '.' = G
mkTile '|' = NS
mkTile '-' = EW
mkTile 'L' = NE
mkTile 'J' = NW
mkTile '7' = SW
mkTile 'F' = SE
mkTile 'S' = Start
mkTile err   = error $ "Unrecognized tile: '" ++ (err : "'")

tileToBox :: Tile -> Char
tileToBox G = ' '
tileToBox NS = '│'
tileToBox EW = '─'
tileToBox NE = '└'
tileToBox NW = '┘'
tileToBox SW = '┐'
tileToBox SE = '┌'
tileToBox NonPath = '·'
tileToBox Start = 'S'

nextCoord :: Coord -> Tile -> Maybe Coord
nextCoord (C 1 0) NS    = Just north
nextCoord (C (-1) 0) NS = Just south
nextCoord (C 0 1) EW    = Just west
nextCoord (C 0 (-1)) EW = Just east
nextCoord (C (-1) 0) NE = Just east
nextCoord (C 0 1) NE    = Just north
nextCoord (C 0 (-1)) NW = Just north
nextCoord (C (-1) 0) NW = Just west
nextCoord (C 1 0) SW    = Just west
nextCoord (C 0 (-1)) SW = Just south
nextCoord (C 1 0) SE    = Just east
nextCoord (C 0 1) SE    = Just south
nextCoord _ _           = Nothing

mkPaths :: Puzzle -> [[Coord]]
mkPaths puz@(board,_) =
  let nexts = nextsFromStart puz in
  mapMaybe (loop []) nexts
  where
    loop acc tileInfo = do
      tileInfo'@((c,t), _) <- nextStep board tileInfo
      if t == Start
      then Just acc else loop (c:acc) tileInfo'

nextsFromStart :: Puzzle -> [((Coord, Tile), Coord)]
nextsFromStart (board,start) =
  map (\(nToStart,(ncoord,tile)) -> ((ncoord,tile), nToStart)) e3
  where
    e2 =
      map (\c -> (c, board M.! c))
      . filter (\c@(C y x) -> y >= 0 && x >= 0 && (board M.! c /= G)) . neighbors $ start
    e3 = map (\(ncoord,t) -> (start - ncoord, (ncoord,t))) e2

nextStep :: Board -> ((Coord,Tile), Coord) -> Maybe ((Coord,Tile), Coord)
nextStep board ((curPos, tile), from) = do
  delta <- nextCoord from tile
  let nextC = delta + curPos
  let nextT' = board M.! nextC
  pure ((nextC, nextT'), delta * (-1))

pboard :: Bool -> IO ()
pboard  b = do
  raw <- if b then getSample 10 else getPuzzle 10
  let puz@(board, start) = parse raw
  let pset = S.insert start . S.fromList . concat . mkPaths $ puz
  let b' = M.toList board
  ptiles board start pset b'
  putStrLn ""
  where
    ptiles :: Board -> Coord -> S.Set Coord -> [(Coord, Tile)] -> IO ()
    ptiles _ _ _ [] = putStr ""
    ptiles board start pset (x:rest) = do
      ptile board start pset x
      ptiles board start pset rest

ptile :: Board ->  Coord -> S.Set Coord -> (Coord, Tile) -> IO ()
ptile board start pmap (loc@(C _ x), t) = do
  putStr $ if x == 0 then "\n" else ""
  if S.member loc pmap
  then do
    if (board M.! loc) `elem` [NS,NE,NW] || (loc == start && S.member (start + north) pmap) then
      setSGR [SetColor Foreground Vivid Red]
    else
      setSGR [SetColor Foreground Vivid Green]
    putStr [tileToBox t]
  else do
    setSGR [SetColor Foreground Dull White]
    putStr "·"