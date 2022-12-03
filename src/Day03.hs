{- DAY03 : https://adventofcode.com/2022/day/3 -}
module Day03 where

import Util (getFile)
import Data.Char (ord)
import Data.List.Split (chunksOf)

score :: Char -> Int
score c | c > 'Z'   = ord c - 96
        | otherwise = ord c - 38

-- | Find the intersection between two lists of the same
--   type, where that type has an instance of Eq
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] = const []
intersect xs = filter (`elem` xs)

-- | Part A - split each line in half, find the intersection.
--   there may be more than one instance of the same Char in 
--   the intersection, so take the head, get the score, take the sum
sln03A :: String -> Int
sln03A =
  let inHalf s = splitAt (length s `div` 2) s in
  sum . map (score . head . uncurry intersect . inHalf) . lines

-- | Part B - take the list of lines as chunks of 3, determine
--   the group score for each chunk, take the sum
sln03B :: String -> Int
sln03B = sum . map grpScore . chunksOf 3 . lines

-- | Calc the group score. Group will be 3 lines long exactly, find
--   the intersection of 2 of the lines, and the intersection of that
--   with the third line. Again, there may be > 1 of the same char 
--   in the intersection, so take the head, and get the score
grpScore :: [String] -> Int
grpScore [a,b,c] = score . head . intersect a $ intersect b c
grpScore _ = error "ASSERT FAILED"

-- | Applies the solution (either sln03A or sln03B) to the puzzle data
solve03 :: (String -> Int) -> IO ()
solve03 fn = do
  txt <- getFile "day03.txt"
  print (fn txt)