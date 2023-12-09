module Y2023.Day09 (sln2309) where

import Util (getNums)
import Data.Set (fromList, singleton)

sln2309 :: String -> (Int, Int)
sln2309 s = ((sum . map loop) puz, (sum . map (loop . reverse)) puz)
  where
  puz = (map getNums . lines) s 
  loop l
    | fromList l == singleton 0 = 0
    | otherwise = last l + loop [b - a | (a,b) <- zip l (tail l)]