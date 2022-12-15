{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2022.Day14 (sln14) where

import qualified Data.Set as S
import Data.Set        (Set)
import Data.List       (find)
import Data.List.Split (splitOn)
import Util            (getNums, pairs, Parts (PartA), signOf)

{-- Types & Consts-------------------------------------------------}

type Point   = (Int, Int)
type Slope   = (Int, Int)
type Segment = (Point, Point)

{-- Solutions----------------------------------------------------}

sln14 :: Parts -> String -> Int
sln14 part = sln part . parse

{-| emit "grains of sand" from (500,0) until for PartA they 
    fall into the abyss, or for PartB they accumulate on an
    infinite plane that exists at y = maxY + 2, until they
    pile all the way up to (500,0) -}
sln :: Parts -> S.Set Point -> Int
sln part points =
  go 0 points (500,0)
  where
    maxY = let adjust = if part == PartA then 0 else 2 in
            (+ adjust) . maximum . map snd . S.toList $ points
    go :: Int -> Set Point -> Point -> Int
    go acc pts p
      | p `S.member` pts = acc
      | otherwise =
        case find (\p' -> not $ p' `S.member` pts) (nexts p) of
          Nothing ->
            go (acc+1) (S.insert p pts) (500,0)
          Just p'@(_,y') ->
            if y' == maxY then
              if part == PartA
              then acc
              else go (acc+1) (S.insert p pts) (500,0)
            else go acc pts p'

{-- Helpers -----------------------------------------------------}

{-| parse the input into a set of points by first generating line
    segments, then projecting the points that would be in each
    segment. Luckily AoC keeps everything in whole coordinates -}
parse :: String -> Set Point
parse = S.fromList
      . concatMap segPoints
      . concatMap ((pairs . map parsePoint) . splitOn " -> ")
      . lines
  where
    parsePoint ps = let [x,y] = getNums ps in (x,y)

{-| get the points in a line segment -}
segPoints :: Segment -> [Point]
segPoints locs@(l1,l2) =
  go [l1]
  where
    (dx,dy) = segSlope locs
    go :: [Point] -> [Point]
    go [] = []
    go (h@(x,y):t) =
      let h' = (x+dx,y+dy) in
      if h' == l2 then h':h:t
      else go (h':h:t)

{-| find the slope of a line segment normalized to 1 -}
segSlope :: Segment -> Slope
segSlope ((x1,y1),(x2,y2)) =
  let (dx',dy') =
        case (x1-x2,y1-y2) of
          (0 , _) -> (0, 1)
          (_ , 0) -> (1, 0)
          (dx,dy)
            | abs dx > abs dy -> (dx `div` dy, 1)
            | otherwise       -> (1, dy `div` dx)
  in (signOf dx' (x2-x1), signOf dy' (y2-y1))

-- | next possible places for a grain of sand
nexts :: Num a => (a,a) -> [(a,a)]
nexts (x,y) = [(x',y+1) | x' <- [x, x-1, x+1]]
