{- DAY03 : https://adventofcode.com/2022/day/3 -}
module Day03 (sln03A, sln03B, solve03) where

import Util             (getFile, intersect)
import Data.Char        (ord)
import Data.List.Split  (chunksOf)

{-| Calculate the score as offsets of ascii values -}
score :: Char -> Int
score c | c > 'Z'   = ord c - 96
        | otherwise = ord c - 38

{-| Part A - split each line in half, find the intersection.
    there may be more than one instance of the same Char in 
    the intersection, so take the head, get the score, take the sum -}
sln03A :: String -> Int
sln03A =
  let half s = let (l,r) = splitAt (length s `div` 2) s in [l,r] in
  sum . map (score . head . intersect . half) . lines

{-| Part B - take the list of lines as chunks of 3, determine
    the group score for each chunk, take the sum -}
sln03B :: String -> Int
sln03B = sum . map (score . head . intersect) . chunksOf 3 . lines

-- | Applies the solution (either sln03A or sln03B) to the puzzle data
solve03 :: (String -> Int) -> IO ()
solve03 fn = do
  txt <- getFile "day03.txt"
  print (fn txt)