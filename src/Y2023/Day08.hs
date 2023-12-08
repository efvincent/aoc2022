{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2023.Day08 (sln2308) where

import Data.List.Split (splitOn)
import qualified Data.Map as M (Map, fromList, keys, (!))


type Ident = String
type Node = (Ident, Ident)
type Network = M.Map Ident Node
type Instructions = String
type Puzzle = (Instructions, Network)

next :: Char -> (a, a) -> a
next 'L' = fst
next 'R' = snd
next _   = undefined

{-- Solutions----------------------------------------------------}

sln2308 :: String -> (Int,Int)
sln2308 s =
  let puz = parse s in
  (solve1 puz, solve2 puz)

solve1 :: Puzzle -> Int
solve1 (instr,net) = findPathLen (== "ZZZ") net (net M.! "AAA") instr

solve2 :: Puzzle -> Int
solve2 (instr,net) =
  let starts = filter ((== 'A') . (!! 2)) . M.keys $ net in
  lcmm (paths [] starts)
  where
    paths :: [Int] ->  [String] -> [Int]
    paths acc [] = acc
    paths acc (start:rest) = 
      let dist = findPathLen ((== 'Z') . (!! 2)) net (net M.! start) instr in
      paths (dist:acc) rest

{-| find length of the path through the network from the starting node
    until the end condition met by the predicate is met -}
findPathLen :: (Ident -> Bool) -> Network -> Node -> Instructions -> Int
findPathLen predicate net startNode instr = loop 0 startNode instr
  where
    loop :: Int -> Node -> Instructions -> Int
    loop acc cur [] = loop acc cur instr
    loop acc cur (i:rest) =
      let nextId = next i cur in
      if predicate nextId then acc + 1 else loop (acc+1) (net M.! nextId) rest

{-| find least common multiple of a list of numbers -}
lcmm :: [Int] -> Int
lcmm xs
  | length xs < 2 = undefined
  | otherwise =
    let (a:b:rest) = xs in
    let n = lcm a b in
    if null rest then n else lcmm (n:rest)

{-- Parsing ----------------------------------------------------}

parse :: String -> Puzzle
parse s =
  let [instructions,p2] = splitOn "\n\n" s in
  let net = M.fromList . map parseLine . lines $ p2 in
  (instructions, net)
  where
    parseLine :: String -> (Ident, Node)
    parseLine l = (take 3 l, (take 3 . drop 7 $ l, take 3 . drop 12 $ l))