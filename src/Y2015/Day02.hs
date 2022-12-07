module Y2015.Day02 where
import Util (tup3, getNums)

calc :: (Int, Int, Int) -> Int
calc (l,w,h) = 
  let 
    s1 = (2 * l * w)
    s2 = (2 * w * h)
    s3 = (2 * h * l)
  in s1 + s2 + s3 + minimum (map (`div` 2) [s1,s2,s3])

sln1502A :: String -> Int
sln1502A = sum . map (calc . tup3 . getNums) . lines 