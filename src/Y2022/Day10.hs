{-# OPTIONS_GHC -Wno-unused-imports #-}
module Y2022.Day10 where

import qualified Data.Set as S
import Util (getSample, getPuzzle)
import Debug.Trace (trace)

samp :: IO String
samp = getSample 10
puzz :: IO String
puzz = getPuzzle 10


data Instr = NOOP | ADDX Int deriving Show

parse :: String -> [Instr]
parse = map parseLine . lines
  where
    parseLine ['n','o','o','p'] = NOOP
    parseLine s = ADDX . read . drop 5 $ s

interpret :: [Instr] -> Int
interpret =
  {-
  go acc cyc cDown xReg toAdd instr -}
  sum . go []  0   0     1    0

  where
    -- samp ans :     21  19   18   21  16   18
    sig = S.fromList [20, 60, 100, 140, 180, 220] :: S.Set Int
    go :: [Int] -> Int -> Int -> Int -> Int -> [Instr] -> [Int]

    -- No more instructions case. Checking that we might still have a countdown
    -- and a toAdd
    go acc cyc cDown xReg toAdd [] =
      let cyc' = cyc + 1
          acc' =
            if S.member cyc' sig 
            then trace (" << A cyc:" ++ show cyc' ++ " xReg:" ++ show xReg ++ ">> ")  (cyc' * xReg):acc
            else acc in
      if cDown == 0
      then acc'
      else go acc' cyc' (cDown-1) 0 0 []
      
    go acc cyc 0     xReg toAdd instrs = 
      let cyc' = cyc 
          acc' = acc
            -- if S.member cyc' sig 
            -- then trace (" << B cyc:" ++ show cyc' ++ " xReg:" ++ show xReg ++ ">> ")  (cyc' * xReg):acc
            -- else acc
          (cDown', toAdd', instrs') = 
            case instrs of
              (NOOP  :rest) -> (1, 0, rest)
              (ADDX n:rest) -> (2, n, rest)
      in go acc' cyc' cDown' (xReg+toAdd) toAdd' instrs'

    go acc cyc cDown xReg toAdd instrs =
      let cyc' = cyc + 1
          acc' = 
            if S.member cyc' sig
            then trace (" << C cyc:" ++ show cyc' ++ " xReg:" ++ show xReg ++ ">> ")  (cyc' * xReg):acc
            else acc
      in go acc' cyc' (cDown-1) xReg toAdd instrs 

    -- go acc cyc cDown x 
    