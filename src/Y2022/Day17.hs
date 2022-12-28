{-|
Module      : Day17
Description : Advent of Code Day 17
Copyright   : (c) Eric Vincent, 2022
License     : ISC
Maintainer  : info at efvincent dot com
Stability   : experimental

This is the 3rd major rewrite of Day 17. Original approached worked
for part A, but the shortcuts and hackery got me in trouble for part B.
There are  a few changesd from the last few attempts:

  - using a better developed @Coord@ module (I also have a @Coord3@ for 3d)
  - also using a dedicated search module with breadth  & depth first search 
  - using the lazy @iterate@ which will generate a list of results
  - the top will start at zero instead of the bottom being zero
  - attempting some optimization with arrays
    - we'll use indexing instead of induction over cycled/infinite lists
  - because we're chainging to Coord with row-major indexing, the initial
    position of coords in rocks is in C y x order. 
  - also since we're starting with zero at the top, we put the rocks just
    outside (negative y) as initial position which is changed when the
    rock comes into play
  - storing the "jets" as a Coord each for left (@west@) and right (@east@),
    when "jetting" rocks we'll just add the jet coord to each rock coord
-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use underscore" #-}

module Y2022.Day17 (sln17, test17) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Util (getPuzzle, Parts (..))
import Coord ( Coord(..), west, east, south, neighbors, cRow, cCol )
import Data.Array (Array(..), listArray, (!), Ix (rangeSize), bounds)
import Search (dfsN)

{-- Types ----------------------------------------------------}

type Rock = Set Coord
type Board = Set Coord

type Jets = Array Int Coord

{-| state of the board, which indexes a particular board by the index
    of the rocks (where we are in the cycle of rocks) and the index
    of jets (where we are in the cycle of jets) -}
type GameState =
  ( Int         -- ^ rock index
  , Int         -- ^ jet index
  , Set Coord)  -- ^ board

{-- Solutions & Tests-----------------------------------------}

-- | fits into the @Util.solve@ function
sln17 :: Parts -> String -> Int
sln17 PartA = slnA 2022
sln17 PartB = slnB

{-| part A is just playing 2022 rocks, then see how high the pile is -}
slnA :: Int -> String -> Int
slnA iters s = getHeightAt (iterate (playRock (parse s)) (0, 0, initBoard)) iters

{-| part B took me a long time and drove 2 compete rewrites of Day 17. 
    remembering that @iterate@ is lazy, we create an infinite list of
    solutions, and can index the state at any particular rock by using 
    the list indexer @(!!)@. 
    
    We call @findCycle@ over normalized boards - @normalize@ makes the top of
    the board at @y == 0@ and everything else is relative to that. This, along
    with the @trim@ that happes during @playRock@, makes it possible to derive
    a map index from the indexes of the rocks and jets, and the entire state
    of the board at any step.

    Once we know where (in terms of rock index) the cycle starts and ends, 
    we can basically calculate how many complete cycles it would take to reach a 
    trillion rocks being dropped, accounting for the fact that the game doesn't
    start right at the start of a cycle, it starts somewhere in the middle of
    a cycle, so we have to account for the remainder. -}
slnB :: String -> Int
slnB s =
  let jets          = parse s
      height        = getHeightAt states
      states        = iterate (playRock jets) (0, 0, initBoard)
      (cStart,cEnd) = findCycle [(rockIdx,jetIdx,normalize board) | (rockIdx,jetIdx,board) <- states]
      cLen          = cEnd - cStart
      cCount        = (1_000_000_000_000 - cStart) `div` cLen
      cRemainder    = (1_000_000_000_000 - cStart) `mod` cLen
      cHeight       = height cEnd - height cStart
  in (height (cEnd + cRemainder) + cHeight * (cCount - 1))


{-| Primary driving function, advances game state by one rock. Note that it
    also calls @trim@ to trim off rows that cannot be reached. Without
    this optimization, part B runs out of memory before it can complete. -}
playRock
  :: Jets
  -> GameState
  -> GameState
playRock jets (rockIdx,jetIdx,board) =
  let (landed, jetIdx') = step jetIdx start
  in (rockIdx', jetIdx', trim (S.union board landed))
  where
    inbounds (C _ x) = 0 <= x && x <= 6
    rockIdx'         = succ rockIdx `mod` 5
    start            = moveRock (rocks ! rockIdx) (C (-boardHeight board-4) 2)
    hasLanded rock   = not (all inbounds rock && S.disjoint board rock)

    step :: Int -> Rock -> (Rock, Int)
    step dj p1
      | S.disjoint board p4 = step dj' p4
      | otherwise = (p3, dj')
      where
        dj' = (dj + 1) `mod` rangeSize (bounds jets)
        p2 = moveRock p1 (jets ! dj)
        p3 | hasLanded p2 = p1
           | otherwise    = p2
        p4 = moveRock p3 south

{-| after the puzzle is complete, I typically create a test function so I can refactor
    solutions and verify that I haven't broken it -}
test17 :: IO ()
test17 = do
  s <- getPuzzle 17
  let sa = slnA 2022 s
      sb = slnB s
  if sa == 3186
  then putStrLn   "Part A - PASS"
  else putStrLn $ "Part A - FAIL ... expected 3186, got " ++ show sa
  if sb == 1566376811584
  then putStrLn   "Part B - PASS"
  else putStrLn $ "Part B - FAIL ... expected 1566376811584, got " ++ show sb

{-- Parsing and Initialization------------------------------}

parse :: String -> Jets
parse s =
  let pc '<' = west; pc '>' = east
      jets = map pc s
  in listArray (0, length jets - 1) jets

initBoard :: Board
initBoard = S.fromList [C 0 x | x <- [0..6]]

rocks :: Array Int Rock
rocks = listArray (0,4) [
    S.fromList [C 0 0,    C 0 1,    C 0 2,    C 0 3             ],
    S.fromList [C (-2) 1, C (-1) 0, C (-1) 2, C 0 1             ],
    S.fromList [C 0 0,    C 0 1,    C 0 2,    C (-1) 2, C (-2) 2],
    S.fromList [C 0 0,    C (-1) 0, C (-2) 0, C (-3) 0          ],
    S.fromList [C (-1) 0, C (-1) 1, C 0 0,    C 0 1             ]]

{-- Helpers----------------------------------------------------}

-- | Renumber a tower so that it's top starts at 0
normalize :: Board -> Board
normalize board = moveRock board (C (boardHeight board) 0)

{-| Finds a repeating cycle and returns the starting and ending rock counts
    (the number of rocks dropped to produce a state) for where the cycle 
    starts and ends.
    
    It does this by looping through all the states, writing the starting
    rock index into a map keyed by the (hash of) the game state itself, which
    includes the rock index, jet index, and the entire board (set of coords). 
    If it finds the same hash, then it returns the start and end rock indexes,
    which are effectively which rock produced the start of the cycle, and which
    rock ended the cycle.  -}
findCycle :: [GameState] -> (Int,Int)
findCycle = go M.empty 0
  where
    go :: Map GameState Int -> Int -> [GameState] -> (Int,Int)
    go _ _ [] = error "no cycle"
    go seen endRockIdx (state:states) =
      case M.lookup state seen of
        Nothing           -> go (M.insert state endRockIdx seen) (endRockIdx + 1) states
        Just startRockIdx -> (startRockIdx, endRockIdx)

-- | Given the game states, determine the height at a particular rock number
getHeightAt :: [GameState] -> Int -> Int
getHeightAt states rockNum  =
  let (_,_,board) = states !! rockNum
  in boardHeight board

-- | height of the board
boardHeight :: Board -> Int
boardHeight b = negate . cRow $ minimum b

-- | moves a rock (set of Coord) by the vector second parameter
moveRock :: Rock -> Coord -> Rock
moveRock rock m = S.mapMonotonic (m +) rock

-- | trims the layers of board that are no longer reachable (optimization)
trim :: Board -> Board
trim board = S.filter live board
  where
    mny      = cRow (minimum board)
    step c   = [n | n <- neighbors c, 0 <= cCol n, cCol n <= 6, cRow c >= mny, S.notMember n board]
    air      = dfsN step [C mny x | x <- [0..6], S.notMember (C mny x) board]
    live b   = any (`elem` air) (neighbors b) || cRow b == mny
