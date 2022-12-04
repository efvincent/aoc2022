{- DAY01 : https://adventofcode.com/2022/day/1 -}
module Day01 (sln01A, sln01B) where

import Data.List.Split (splitOn)
import Data.List (sortBy)

{-| 

    algo:

    1) split the input string by \n\n (splitOn)
    2) for each of those
      2.1) split it into words (whitespace delimited strings)
      2.2) for each of those
        2.2.1) parse them as integers (read)
      2.3) sum the list of integers
    
    3) we now have [Int] where each Int is the calories carried by an elf
    4) for part 1, take the maximum
    5) for part 2:
      5.1) sort the list descending (sortBy takes an ordering function, that
                                function takes two ordered values and
                                returns one of LT GT EQ. The compare function
                                does this for ascending order. If we flip
                                the arguments of compare, we get descending
                                order sort)
      5.2) take the top 3 from the list (now the top three biggest)
      5.3) sum the top 3

-}
sln :: String -> (Int,Int)
sln s = 
  let xs = map ((sum . map read) . words) . splitOn "\n\n" $ s in
  (maximum xs, sum . take 3 . sortBy (flip compare) $ xs)

sln01A :: String -> Int
sln01A = fst . sln

sln01B :: String -> Int
sln01B = snd . sln