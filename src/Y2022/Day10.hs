{- DAY10 : https://adventofcode.com/2022/day/10 -}
module Y2022.Day10 (sln10A, sln10B) where

import qualified Data.Set as S
import Data.List.Split (chunksOf)
import Data.List (intercalate)

{-- Types & Consts-------------------------------------------------}

-- | only two operations, no op does nothing, ADDX adds a value to the X register
data Instr = NOOP | ADDX Int deriving Show

onPixel :: Char
onPixel = '\11036'

offPixel :: Char
offPixel = '\11035'

{-- Solutions -----------------------------------------------------}

{-| same basic solution for both parts; simulate the machine running,
    capture the value of the X register on every tick. See individual
    part solutions below for what happens next... -}
sln :: [Instr] -> [(Int, Int)]
sln = reverse . go [] 0 0 1 0
  where
    go :: [(Int,Int)] -> Int -> Int -> Int -> Int -> [Instr] -> [(Int,Int)]
    -- base case, no more instructions
    go acc cyc _ _ xReg [] = (cyc,xReg):acc
    
    {- count down == 0, current op has processed. Apply `toAdd` to X register,
       determine next instruction, set new count down and toAdd values, 
       recursively call next go with the tail of the instruction list -}
    go acc cyc _cDown@0 xReg toAdd instrs = 
      let (cDown', toAdd', instrs') = 
            case instrs of
              (NOOP  :rest) -> (1, 0, rest)
              (ADDX n:rest) -> (2, n, rest)
      in go acc cyc cDown' (xReg+toAdd) toAdd' instrs'
    
    {- count down is not zero, we're still processing an op. Tick the cycle, 
       decrement count down, recursively call go w/o consuming an instruction -} 
    go acc cyc cDown xReg toAdd instrs =
      let acc' = (cyc,xReg):acc 
          cyc' = cyc + 1
      in go acc' cyc' (cDown-1) xReg toAdd instrs 

{-| In part A, we sample the X register at 6 different clock cycles by checking
    the cycle number against a set of sample points. Then we multiply each pair
    of (cycle,X-Register), and sum those products -}    
sln10A :: String -> Int
sln10A =
  let sig = S.fromList [20, 60, 100, 140, 180, 220] in 
  sum . map (uncurry (*)) . filter (flip S.member sig . fst) . sln . parse

sln10B :: String -> String
sln10B = intercalate "\n" . chunksOf 40 . map charAt . sln . parse
  where
    charAt (cyc, s) = 
      let cyc' = cyc `mod` 40 in 
      if cyc' `elem` [s+1,s,s-1] 
      then onPixel
      else offPixel

{-- Helpers -----------------------------------------------------}

parse :: String -> [Instr]
parse = map parseLine . lines
  where
    parseLine ['n','o','o','p'] = NOOP
    parseLine s = ADDX . read . drop 5 $ s