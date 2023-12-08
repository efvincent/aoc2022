{-# OPTIONS_GHC -Wno-unused-imports #-}
module Y2022.Day18 (sln18, test18) where

import Util (getSample, getPuzzle, Parts(..))
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.Foldable (minimumBy, maximumBy, toList)
import Data.Maybe (fromMaybe, isNothing, fromJust)
import Coord3 (Coord3(..), neighbors, from3Tuple, boundingBox)
import GHC.Ix (inRange)
import Search (dfs)

type Coords = Set Coord3

{-- Solutions----------------------------------------------------}

sln18 :: Parts -> String -> Int
sln18 PartA = slnA . parse
sln18 PartB = slnB . parse

{-| For part A, we need count the number of sides of each cube not
    immediately connected to another cube. We map all cubes over
    the @sidesUnconnectedTo@ function and get the length -}
slnA :: Coords -> Int
slnA cubes =
  sum . map (sidesUnconnectedTo cubes) . toList $ cubes

{-| For part B, we're supposed to consider the number of sides of
    cubes not in contact with "trapped air", which is a pocket of
    air completely surrounded by cubes. This may include a pocket
    that's made up of more than one cube of space. See @getAir@ for
    the logic of finding those "trapped air" cubes. Once we have them
    we generate the list of all cubes who's neighbors are members
    of trapped air. -}
slnB :: Set Coord3 -> Int
slnB cubes =
  let air = getAir cubes in
  length [() | c <- S.toList cubes, n <- neighbors c, S.member n air]

{-- Tests   -----------------------------------------------------}

test18 :: IO ()
test18 = do
  raw <- getPuzzle 18
  let p = parse raw
      sa = slnA p
      sb = slnB p
  if sa == 4242
  then putStrLn "Part A - PASS"
  else putStrLn $ "Part B - Fail ... expected 4242, got " ++ show sa
  if sb == 2428
  then putStrLn "Part B - PASS"
  else putStrLn $ "Part B - Fail ... expected 2428, got " ++ show sb
  
{-- Helpers -----------------------------------------------------}

getAir :: Coords -> Coords
getAir cubes =
  let (mn, mx) = fromJust . boundingBox $ cubes
      box = (mn - 1, mx + 1)
      step c = [n | n <- neighbors c, inRange box n, S.notMember n cubes]
  in S.fromList (dfs step (mx + 1))

parse :: String -> Coords
parse = S.fromList . map (from3Tuple . read . (\s -> "(" ++ s ++ ")")) . lines

sidesUnconnectedTo :: Coords -> Coord3 -> Int
sidesUnconnectedTo others c =
  let cs = S.fromList . neighbors $ c
  in length (cs \\ others)