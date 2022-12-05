{- DAY05 : https://adventofcode.com/2022/day/4 -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day05 where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import qualified Data.Map as M (Map, fromList, insert, toList, (!))
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))
import Util (getPuzzle)

type Stack = (Int, String)

type Stacks = M.Map Int String

type Move = (Int, Int, Int)

makeStacks :: String -> Stacks
makeStacks raw =
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

makeMoves :: String -> [Move]
makeMoves =
  map mi . lines
  where
    mi s =
      let [n, f, t] = getNums s
       in ( n,  f,  t)

getNums :: String -> [Int]
getNums s =
  map read $ getAllTextMatches (s =~ ("[0-9]+" :: String))

move :: Bool -> Move -> Stacks -> Stacks
move partA (n, f, t) ss =
  let f' = ss M.! f
      t' = ss M.! t
      fn = if partA then reverse else id
      s  = fn . take n $ f'
   in M.insert f (drop n f') (M.insert t (s ++ t') ss)

applyMoves :: Bool -> Stacks -> [Move] -> String
applyMoves partA stacks = map (head . snd) . M.toList . go stacks
  where
    go acc [] = acc
    go acc (h : t) =
      let acc' = move partA h acc
       in go acc' t

solve05 :: IO (String,String)
solve05 = do
  raw <- getPuzzle 5
  let [rawStacks, rawMoves] = splitOn "\n\n" raw
  let stacks = makeStacks rawStacks
  let instrs = makeMoves rawMoves
  pure (applyMoves True stacks instrs, applyMoves False stacks instrs)
