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