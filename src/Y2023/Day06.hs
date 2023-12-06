{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2023.Day06 (sln2306) where

import Util (getNums, Parts(..))
type Time   = Int
type Dist   = Int
type Race   = (Time, Dist)
type Puzzle = [Race]
type CompFn = Int -> Int -> Bool

sln2306 :: String -> (Int,Int)
sln2306 s =
  let solve = product . map run in
  (solve (parse s PartA), solve (parse s PartB))

run :: Race -> Time
run race =
  let first = loop (>) 0 race in
  let second = loop (<=) first race in
  second - first
  where
    loop :: CompFn -> Time ->  Race -> Time
    loop compFn holdTime (raceTime, distRecord)
      | holdTime >= raceTime = 0
      | (holdTime * (raceTime - holdTime)) `compFn` distRecord = holdTime
      | otherwise = loop compFn (holdTime + 1) (raceTime, distRecord)

parse :: String -> Parts -> Puzzle
parse s p = 
  let ls = lines s in
  case p of
    PartA -> zip (getNums $ head ls) (getNums $ ls !! 1)
    PartB -> 
      let mkOne = (:[]) . read . foldr1 (++) . map show . getNums in
      zip (mkOne $ head ls) (mkOne $ ls !! 1)