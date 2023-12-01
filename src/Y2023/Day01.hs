{-# OPTIONS_GHC -Wno-type-defaults #-}
module Y2023.Day01 (sln) where

import Data.Char (isNumber)
import Data.Bifunctor (first)

digits :: [(String, Int)]
digits =
  zip [ "one", "two", "three", "four", "five" , "six", "seven", "eight", "nine" ] [1..9]
  ++ zip (map show [0..9]) [0..9] 

stigid :: [(String, Int)]
stigid = map (first reverse) digits

sln :: String -> (Int,Int)
sln s = (sln01A s, sln01B s)

sln01A :: String -> Int
sln01A  =
  sum . map ((\s -> read [head s, last s]) . filter isNumber) . lines

sln01B :: String -> Int
sln01B =
  sum . map (\s -> firstDigit s digits * 10 + firstDigit (reverse s) stigid) . lines
  where
    startsWithAny [] _ = Nothing
    startsWithAny ((txt,n):rest) t =
      if take (length txt) t == txt then Just n else startsWithAny rest t

    firstDigit txt tests =
      if null txt then undefined else
      case startsWithAny tests txt of
        Just n -> n
        Nothing -> firstDigit (drop 1 txt) tests
