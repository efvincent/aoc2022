module Y2023.Day10 where

import Data.List.Split (splitOn)
import qualified Data.Map as M (Map, fromList, keys, (!))


type Puzzle = Int

{-- Solutions----------------------------------------------------}

sln2310 :: String -> (Int,Int)
sln2310 s =
  let puz = parse s in
  (solve1 puz, solve2 puz)

solve1 :: Puzzle -> Int
solve1 puz = undefined

solve2 :: Puzzle -> Int
solve2 puz = undefined


{-- Parsing ----------------------------------------------------}

parse :: String -> Puzzle
parse s =
  undefined