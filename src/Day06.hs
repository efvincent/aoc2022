{- DAY06 : https://adventofcode.com/2022/day/6 -}
module Day06 (sln06A, sln06B) where

import Data.Set (fromList)
import Util     (Parts(..))

{-| Solution for both parts is to find the first sequence of N characters
    in a string that do not repeat. To do this we: 
    
    - @take@ the first N chars from the string param @s@, make a @Set@ from them
    - if that set has length N then: 
      + return the position of last char, which is @idx@ + N 
    - else we there's at least one repeat, so:
      + call sln incrementing @idx@ and passing the tail of the input string @s@
    -}
sln :: Parts -> Int -> String -> Int
sln _ _ [] = error "signal not found in input string"
sln part idx s =
  let len = if part == PartA then 4 else 14 in
  if (length . fromList $ take len s) == len 
  then idx + len - 1
  else sln part (idx + 1) (tail s)

-- | Call the solution for part A, start counting index at 1
sln06A :: String -> Int
sln06A = sln PartA 1

-- | Call the solution for part B, start counting index at 1
sln06B :: String -> Int
sln06B = sln PartB 1