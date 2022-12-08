{-# OPTIONS_GHC -Wno-unused-imports #-}
module Y2022.Day08 where

import qualified Data.Map as M
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.List (foldl')
import Util ( getSample, getPuzzle )
import Debug.Trace (trace)

samp :: IO String
samp = getSample 8
puzz :: IO String
puzz = getPuzzle 8

type Grid = V.Vector (V.Vector Int)
type Loc = (Int,Int)
type Seen = S.Set Loc
type Scores = M.Map Loc Int

parse :: String -> Grid
parse = V.fromList . map rowToVec . lines
  where
    rowToVec :: String -> V.Vector Int
    rowToVec = V.fromList . reverse . foldl' (\acc c -> read [c]:acc) []

treeAt :: Loc -> Grid -> Int
treeAt (x, y) g = (g ! y) ! x

checkRow :: Int -> Grid -> Seen -> Seen
checkRow y g seen =
  let
    maxX = V.length (g ! y)
    seen' = go (+ 1) (== maxX) seen (negate 1) 0 in
  go pred (== (-1)) seen' (-1) (maxX - 1) 
  where
    go :: (Int -> Int) -> (Int -> Bool) -> Seen -> Int -> Int -> Seen
    go next checkStop s tallest x 
      | checkStop x = s
      | otherwise =
        let val = treeAt (x,y) g in
        if val > tallest
        then go next checkStop (S.insert (x,y) s) val     (next x) 
        else go next checkStop s                  tallest (next x) 

checkCol :: Int -> Grid -> Seen -> Seen
checkCol x' g seen =
  let
    maxY = V.length g
    seen' = go (+ 1) (== maxY) seen (negate 1) 0 in
  go pred (== (-1)) seen' (-1) (maxY - 1)
  where
    go :: (Int -> Int) -> (Int -> Bool) -> Seen -> Int -> Int -> Seen
    go next checkStop s tallest y 
      | checkStop y = s
      | otherwise =
        let val = treeAt (x',y) g in
        if val > tallest
        then go next checkStop (S.insert (x',y) s) val     (next y)
        else go next checkStop s                   tallest (next y) 

check :: Grid -> Seen
check g =
  let seen = foldl' (\acc y -> checkRow y g acc) S.empty [0..(V.length g - 1)] in
  foldl' (\acc x -> checkCol x g acc) seen [0..(V.length (g ! 0) - 1)]

outOfBounds :: Loc -> Grid -> Bool
outOfBounds (x,y) g = x < 0 || y < 0 || x >= V.length (g ! 0) || y >= V.length g

scenic :: Grid -> Loc -> Int
scenic g (x,y) =
    go 0 (x,y-1) (\(x',y') -> (x'     , y' - 1)) 
  * go 0 (x-1,y) (\(x',y') -> (x' - 1 , y'    )) 
  * go 0 (x,y+1) (\(x',y') -> (x'     , y' + 1)) 
  * go 0 (x+1,y) (\(x',y') -> (x' + 1 , y'    ))
  where
    go :: Int -> Loc -> (Loc -> Loc) -> Int
    go acc loc next   
      | outOfBounds loc g = acc
      | otherwise =
        if treeAt loc g >= treeAt (x,y) g then acc + 1
        else go (acc+1) (next loc) next
        
allIdx :: Grid -> [Loc]
allIdx g = [(x,y) | y <- [0..V.length g - 1], x <- [0..V.length (g!0) - 1]]

sln08A :: String -> Int
sln08A = length . check . parse 

sln08B :: String -> Int
sln08B s =
  let g = parse s in
  maximum . map (scenic g) . allIdx $ g