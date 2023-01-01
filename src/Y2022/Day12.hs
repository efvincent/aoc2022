module Y2022.Day12 (sln12) where

import Grid (Grid, grid, nbors4)
import qualified Data.List as List
import Algorithm.Search (aStar, bfs)
import GHC.Arr ((//), assocs, (!))
import Util (Parts (..))

type Hill  = Grid Char
type Point = (Int,Int)

parse :: String -> Hill
parse = grid . lines

sln12 :: Parts -> String -> Int
sln12 part s =
  let g = parse s
      f = if part == PartA then maybe 0 fst . solveA else maybe 0 length . solveB
  in  f g

get :: Char -> Hill -> Point
get c g = case List.find ((== c) . snd) $ assocs g of
  Just (p, _) -> p
  Nothing -> error $ "Could not findOne " ++ show c

prepStartEnd :: Hill -> (Hill, Point, Point)
prepStartEnd g = (g', start, end)
  where
    start = get 'S' g
    end   = get 'E' g
    g'    = g // [(start, 'a'), (end, 'z')]

solveA :: Hill -> Maybe (Int, [Point])
solveA og = aStar
  (\p -> filter (isReachable g p) $ nbors4 g p)
  (\_ _ -> 1)
  (dist end)
  (== end)
  start
  where
    (g, start, end) = prepStartEnd og
    dist (x1,y1) (x2,y2) = abs (x1 - x2) + (y1 - y2)

solveB :: Hill -> Maybe [(Int,Int)]
solveB og = bfs
  (\p -> filter (flip (isReachable g) p) $ nbors4 g p)
  ((== 'a') . (g !))
  end
  where
    (g, _start, end) = prepStartEnd og

isReachable :: Hill -> Point -> Point -> Bool
isReachable g p1 p2 = succ (g ! p1) >= g ! p2
