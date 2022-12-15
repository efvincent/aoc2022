{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Y2022.Day15 (sln15) where

import Util (getNums, Parts (..))
import Data.List (foldl', sort, nub)
import Data.Maybe (isJust)

{-- Types & Consts-------------------------------------------------}

type Point = (Int, Int)
type Sample = (Point, Point)
type Range = Point


{-- Solutions----------------------------------------------------}

sln15 :: Parts -> String -> Int
sln15 part s =
  let puzz = parse s in
  if part == PartA 
  then slnA 2_000_000 puzz
  else slnB 4_000_000 puzz

slnA :: Int -> [Sample] -> Int
slnA y_target sample = 
  flip (-) beacons                      -- subtract out number of beacons in the range
  . sum                                 -- get the sum of counts at each range
  . map (\(s,e) -> e - s + 1)           -- count number of covered points 
  . getXRangeAtY y_target $ sample      -- get covered X range at target y 
  where
    beacons = length . nub . filter ((== y_target) . snd) . map snd $ sample

slnB :: Int -> [Sample] -> Int
slnB maxY samples = 
  (\[Just (x,y)] -> x * multiplier + y) -- calc AoC answer (assumes there's exactly 1 answer)
  . filter isJust                       -- drop everything but the answer
  . map (`getInRangeAtY` samples)       -- maps each line into a range that's covered, if covered
  $ [0..maxY]                           -- all horizontal lines (y coordinates)
  where
    multiplier = 4_000_000
    {-| Calculate the points that are in range for all the samples on a particular
        horizontal line at @y_target@. -}
    getInRangeAtY :: Int -> [Sample] -> Maybe Range
    getInRangeAtY y_target ss =
      case getXRangeAtY y_target ss of
        []                    -> Just (0, y_target)
        ((s,e):_) | 0 < s     -> Just (0, y_target)
                  | e < maxY  -> Just (e + 1, y_target)
                  | otherwise -> Nothing

{-- Helpers -----------------------------------------------------}

{-| Find the manhattan distance between two points -}
mhDist :: Point -> Point -> Int
mhDist (x1,y1) (x2,y2)  = abs (x1-x2) + abs (y1-y2)

parse :: String -> [Sample]
parse = map (toPoints . getNums) . lines
  where toPoints [x1,y1,x2,y2] = ((x1,y1),(x2,y2))

{-| Collapses multiple ranges into minimal set of contiguous ranges -}
collapse :: [Range] -> [Range]
collapse = reverse . foldl' go [] . sort
  where
    go :: [Range] -> Range -> [Range]
    go [] r = [r]
    go (accHead@(accStart, accEnd) : accTail) curRange@(start, end)
      | start <= accEnd + 1 = (accStart, max end accEnd) : accTail
      | otherwise           = curRange : accHead : accTail

{-| given a horiz line at y and the samples from input, get the most compact
    set of ranges of "covered" x indexes -}
getXRangeAtY :: Int -> [Sample] -> [Range]
getXRangeAtY y_target samples =
  -- after collecting all the ranges, collapse them into their minimal set
  collapse $
    {-| calculate the individual ranges that each sample "covers" the line at y.
        this is done by calculating manhattan distance for a sample, subtracting
        the difference between the sensor's y and the target y. This has the effect
        of "shrinking" the sensor range to the distance to the target line where
        it intersects the sensor's range. If the line is out of range, @targetDist@
        will be negative and is filtered out -}
    [ (x_sensor - targetDist, x_sensor + targetDist) | (sensor@(x_sensor, y_sensor), target) <- samples,
        let targetDist = mhDist sensor target - abs (y_target - y_sensor),
        targetDist >= 0]