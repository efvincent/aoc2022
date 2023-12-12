{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2023.Day11 (sln2311) where

import qualified Data.Set as S (fromList, member)
import Coord (Coord(..), manhattan)
import Data.Bifunctor (first)

type Puzzle = (([Int], [Int]), [Coord])

{-- Solutions----------------------------------------------------}

sln2311 :: String -> (Int,Int)
sln2311 s =
  let puz = parse s in
  (solve 1 puz, solve 999999 puz)

solve :: Int -> Puzzle -> Int
solve mult puz = 
   let (_, stars) = expand mult puz in
   let pairs = [(s1,s2) | s1 <- stars, s2 <- stars, s1 < s2] in
   sum $ map (uncurry manhattan) pairs

expand :: Int -> Puzzle -> Puzzle
expand mult = expandCols mult . expandRows mult

expandCols :: Int -> Puzzle -> Puzzle
expandCols mult (empties@(_,emptyCols), stars) =
  (empties, map (\(C y x) -> C y (x + (mult * length (filter (< x) emptyCols)))) stars)

expandRows :: Int -> Puzzle -> Puzzle
expandRows mult (empties@(emptyRows,_), stars) =
  (empties, map (\(C y x) -> C (y + (mult * length (filter (< y) emptyRows))) x) stars)

{-- Parsing ----------------------------------------------------}

parse :: String -> Puzzle
parse s =
  let emptyRows = filter anyStarsInRow [0..my] in
  let emptyCols = filter anyStarsInCol [0..mx] in
  ((emptyRows,emptyCols), stars)
  where
    stars = concatMap parseLine (zip [0..] . lines $ s)
    starSet = S.fromList stars
    (mx,my) = (maximum $ map (\(C y _) -> y) stars, maximum $ map (\(C _ x) -> x) stars)
    anyStarsInRow y = not $ any ((`S.member` starSet) . C y) [0..mx]
    anyStarsInCol x = not $ any ((`S.member` starSet) . (`C` x)) [0..my]

parseLine :: (Int, String) -> [Coord]
parseLine (y,s) = map fst . filter ((==) '#' . snd) . zipWith (curry (first (C y))) [0..] $ s
