-- https://adventofcode.com/2015/day/1
module Y2015.Day01 (sln1501A, sln1501B) where

sln1501A :: String -> Int
sln1501A = go 1
  where
    go n [] = n
    go n (h:t) | h == '('   = go (n+1) t
                | otherwise = go (n-1) t

sln1501B :: String -> Int
sln1501B = go 1 0
  where
    go :: Int -> Int -> String -> Int
    go _ _ "" = 0
    go f count (c:cs)
      | c == '(' && f /= -1 = go (f + 1) (count + 1) cs
      | f == -1 = count
      | otherwise = go (f - 1) (count + 1) cs