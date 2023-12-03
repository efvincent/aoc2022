{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2023.Day02 (sln2302) where

import Data.List.Split (splitOn)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import qualified Data.Map as M

data Cube = Red | Green | Blue deriving (Show, Eq, Ord)

type Cubes = M.Map Cube Int

type Game = (Int, [Cubes])

sln2302 :: String -> (Int, Int)
sln2302 s = (solve1 s, solve2 s)

solve1 :: String -> Int
solve1 s =
  let games = parse s in
  let mx = M.fromList [(Red,12),(Green,13),(Blue,14)] in
  sum . map (calcAllowed mx) $ games
  where
    calcAllowed :: Cubes -> Game -> Int
    calcAllowed lim (gid, sets) =
      let mxSet = maxCubes sets in
      if checkSet mxSet lim then gid else 0
      where
        checkSet :: Cubes -> Cubes -> Bool
        checkSet mx cs =
          ((mx M.! Red)   <= (cs M.! Red)) &&
          ((mx M.! Green) <= (cs M.! Green)) &&
          ((mx M.! Blue)  <= (cs M.! Blue))

solve2 :: String -> Int
solve2 =
  sum . map getPow . parse 
  where
    getPow :: Game -> Int
    getPow (_,cubeSet) =
      let [(_,c1),(_,c2),(_,c3)] = M.toList $ maxCubes cubeSet in
      c1 * c2 * c3

maxCubes :: [Cubes] -> Cubes
maxCubes cubes =
  let getMax c = (c, maximum $ map (M.findWithDefault 0 c) cubes) in
  M.fromList [getMax Red, getMax Green, getMax Blue]

parse :: String -> [Game]
parse s =
  let ls = lines s in
  map parseLine ls
  where
    parseCube :: String -> (Cube, Int)
    parseCube rawCube =
      let [n,c] = splitOn " " . dropWhileEnd isSpace . dropWhile isSpace $ rawCube in
      let num = read n in
      case c of
        "red"   -> (Red, num)
        "green" -> (Green, num)
        "blue"  -> (Blue, num)
        _       -> undefined

    parseLine l =
      let [gs,cs] = splitOn ":" l in
      let gid = read (drop 5 gs) :: Int in
      let sets = map (M.fromList . map parseCube . splitOn ",") $ splitOn ";" cs in
      (gid,sets)
