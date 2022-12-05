{- DAY05 : https://adventofcode.com/2022/day/4 -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day05 (solve05) where

import Data.Char                (isDigit)
import Data.List.Split          (splitOn)
import qualified Data.Map as M  (Map, fromList, insert, toList, (!))
import Util                     (getPuzzle, getNums, Parts(..))

{-
The raw input for the puzzle looks like this:

            [D]    
        [N] [C]    
        [Z] [M] [P]
        1   2   3 

        move 1 from 2 to 1
        move 3 from 1 to 3
        move 2 from 2 to 1
        move 1 from 1 to 2

The first section are the "stacks". In this sample, they're stacks
1, 2 and 3. Think of them being stacks of crates in a warehouse. 
The second section is a series of instructions to move some number
of crates from one stack to another. -}

{-| A stack is a numbered list of "cargo", where each cargo is 
    represented by a character -}
type Stack = (Int, String)

{-| All the stacks are managed by a map that indexes each
    stack by its stack number -}
type Stacks = M.Map Int String

{-| A move is represented by a 3-tuple consisting of the number
    of cargo to move, the index of the from stack and the index
    of the to stack -}
type Move = (Int, Int, Int)

{-| Each stack is in a "column" of the raw string. That means
    for each stack, the letters that indicate the cargo in
    the stack are all at the same index in each line of the 
    first raw section. We loop through each stack by index,
    pulling out each character at that index for each line
    in the raw input. Keeping them in order, this forms 
    the stacks, which are then added to the map of all stacks -}
parseStacks :: String -> Stacks
parseStacks raw =
  let rawStacks = lines raw
      maxIdx = last . getNums . last $ rawStacks in
  M.fromList . go maxIdx [] 1 1 $ rawStacks
  where
    chars c = not (c == ' ' || isDigit c)
    go :: Int -> [Stack] -> Int -> Int -> [String] -> [Stack]
    go bi acc idx n rs
      | n > bi = acc
      | otherwise =
          let stack = filter chars . map (!! idx) $ rs
           in go bi ((n, stack) : acc) (idx + 4) (n + 1) rs

{-| the moves are parsed out by using a regex to extract each
    natural number from each line of the raw "moves" string -}
parseMoves :: String -> [Move]
parseMoves =
  map mi . lines
  where
    mi s =
      let [n, f, t] = getNums s
       in ( n,  f,  t)

{-| The move algorithm changes between parts A and B. In each
    case, given the input of (number to move, from, to), we 
    pop 'number to move' elements off the stack indexed by 'from'
    and push them onto the stack indexed by 'to'. 
    For part A: we reverse order of the popped cargo using the
                @reverse@ function, simulating moving the cargo 
                one at a time 
    For part B: the popped cargo stays in the same order, no
                reverse is necessary, the @id@ function is used
                in its place. -}
move :: Parts -> Move -> Stacks -> Stacks
move p (n, f, t) ss =
  let f' = ss M.! f
      t' = ss M.! t
      fn = if p == PartA then reverse else id
      s  = fn . take n $ f'
   in M.insert f (drop n f') (M.insert t (s ++ t') ss)

{-| Applying the moves is a matter of folding over the list of
    Moves applied with the @move@ function to the starting stack -}
applyMoves :: Parts -> Stacks -> [Move] -> String
applyMoves p stacks = map (head . snd) . M.toList . go stacks
  where
    go acc [] = acc
    go acc (h : t) =
      let acc' = move p h acc
       in go acc' t

{-| The solve function pulls together the steps. Can't use the utility
    functions in this case since the answers are strings and not
    @Int@s. I will have to update my utility functions to be more
    flexible. -}
solve05 :: IO (String,String)
solve05 = do
  raw <- getPuzzle 5
  let [rawStacks, rawMoves] = splitOn "\n\n" raw
  let stacks   = parseStacks rawStacks
  let moves    = parseMoves  rawMoves
  let ans part = applyMoves  part stacks moves
  pure (ans PartA, ans PartB)
