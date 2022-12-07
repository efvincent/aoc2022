{- DAY04 : https://adventofcode.com/2022/day/4 -}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2022.Day04 (sln04A, sln04B) where

import Data.Set         (Set, fromList, isSubsetOf, intersection)
import Data.List.Split  (splitOn)

{-| Solve by parsing the string into a list of pairs of sets, then count
    the number of times one set is a subset of the other (part A) or
    the two sets intersect (part B) -}
sln :: Bool -> String -> Int
sln partA s =
  let sets = map (map (expand . map read . splitOn "-") . splitOn ",") . lines $ s
      fn   = if partA then subs else intr
  in  length . filter (\[a,b] -> fn a b) $ sets
  where
    -- Expand a 2 element list into a list ranging from a to b
    expand :: [Int] -> Set Int
    expand [a,b] = fromList $ enumFromTo a b

    -- True when either set is a subset of the other
    subs a b = a `isSubsetOf` b || b `isSubsetOf` a

    -- True when there's an intersection between the sets
    intr a b = not (null (intersection a b))

-- | public facing part A solution function
sln04A :: String -> Int
sln04A = sln True

-- | public facing part B solution function
sln04B :: String -> Int
sln04B = sln False