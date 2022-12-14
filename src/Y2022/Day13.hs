{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2022.Day13 (sln13, puzz, samp) where

import Util            (getNums, Parts (..), getSample, getPuzzle)
import Data.Char (isDigit)
import Data.List.Split (chunksOf)
import Data.List (sort)

data Packet
  = N Int
  | L [Packet]
  deriving (Show, Read, Eq)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (N x) (N y) = compare x         y
  compare (N x)    y  = compare (L [N x]) y
  compare    x  (N y) = compare x         (L [N y])
  compare (L x) (L y) = compare x          y

samp :: IO String
samp = getSample 13
puzz :: IO String
puzz = getPuzzle 13

parse :: String -> [Packet]
parse =
  map parseLine . filter (not . null) . lines

parseLine :: String -> Packet
parseLine = read . go
  where
    go [] = ""
    go ('[' : t) = "L [" ++ go t
    go (x   : t)
      | isDigit x = "N " ++ (x : takeWhile isDigit t) ++ go (dropWhile isDigit t)
      | otherwise = x : go t

sln13 :: Parts -> String -> Int
sln13 part s =
  let puzzle = parse s
      sln    = if part == PartA then slnA else slnB
  in  sln puzzle

slnA :: [Packet] -> Int
slnA = 
  sum . map fst . filter snd . zip [1..] . map (\[x,y] -> x <= y) . chunksOf 2

slnB :: [Packet] -> Int 
slnB ps =
  let (d1, d2) = (L [L [N 2]], L [L [N 6]])
      sorted = sort $ d1 : d2 : ps
      idxOf x = fst . head . filter ((== x) . snd) . zip [1..]    -- start at 1!
  in  idxOf d1 sorted * idxOf d2 sorted