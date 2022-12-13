{-# OPTIONS_GHC -Wno-unused-imports #-}
module Y2022.Day12 where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List       (sort, foldl')
import Fifo            (Fifo, push, pop, fromList)
import Util            (getNums, Parts (..), getSample, getPuzzle)
import Data.Vector     ((!))
import qualified Data.Vector as V
import qualified Data.Set as S

samp :: IO String
samp = getSample 12
puzz :: IO String
puzz = getPuzzle 12

type Grid = V.Vector (V.Vector Char)

type Loc = (Int,Int)

data Checkpoint = Cp 
  { _path :: [Loc]
  , _opts :: [Loc]
  } deriving Show

type BackStack = [Checkpoint]

data Puzzle = Puz 
  { _grid   :: Grid
  , _start  :: Loc
  , _end    :: Loc 
  , _mx     :: Int
  , _my     :: Int
  , _bstack :: BackStack
  } deriving (Show)

parse :: String -> Puzzle
parse s =
  let
    g = V.fromList . map V.fromList . lines $ s in
    Puz { _grid   = g
        , _start  = findInGrid 'S' g
        , _end    = findInGrid 'E' g 
        , _mx     = V.length (g V.! 0) - 1
        , _my     = V.length g - 1 
        , _bstack = [] } 
  where
    findInGrid :: Char -> Grid -> Loc
    findInGrid c g = 
      case go 0 of
        Just p -> p
        Nothing -> error "invalid puzzle data"
      where
        maxIdx = V.length g - 1
        go :: Int -> Maybe Loc
        go y 
          | y >= maxIdx = Nothing
          | otherwise = case V.findIndex (== c) (g V.! y) of
              Nothing -> go (y+1) 
              Just x -> Just (x,y)

{-- algo:

    from the curent point         <- backtrack to here
    get list of choices           <- continue
    are there any viable choices?
      no - backtrack by popping state off the stack
      yes -
        pick first, remove it from choices
        push (current path, unused choices) onto stack
        make move
        is this part of one of the other paths?
          yes - backtrack
        is it a solution?
          yes - is it shorter than current shortest?
            yes - it's the new shortest, backtrack
            no - backtrack
          no - continue

        




-}
findPath =
  undefined


              

