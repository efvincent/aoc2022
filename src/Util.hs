module Util where
import System.Environment (getEnv)

{-| Given a filename gets the contents of that file as a string. 
    requires the environment variable @AOC2022_DATA@ to be set
    to the path (without trailing slash) where the data files
    are located. -}
getFile :: String -> IO String 
getFile file = do
  datapath <- getEnv "AOC2022_DATA"
  let fpath = datapath ++ "/" ++ file
  readFile fpath

{-| Finds the intersection between multiple lists -}
intersect :: Eq a => [[a]] -> [a]
intersect [] = [] 
intersect (h:t) =
  loop h t 
  where
    -- Finds the intersection between two lists
    intr :: Eq a => [a] -> [a] -> [a] 
    intr [] = const []
    intr xs = filter (`elem` xs)

    {- Finds the intersection between multiple lists by
       recursively calling @intr@ on the accumulated 
       intersection and the next list -}
    loop acc [] = acc
    loop acc (h':t') = loop (intr acc h') t'