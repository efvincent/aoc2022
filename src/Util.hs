module Util 
  ( getSample
  , getPuzzle
  , solve
  , sample
  , intersect 
  , getNums
  , startsWith
  , tup2
  , tup3
  , Parts (..)
  ) where
import System.Environment (getEnv)
import Text.Regex.TDFA (AllTextMatches(..), (=~))

data Parts     = PartA  | PartB  deriving (Eq, Ord, Show)
data InputType = Sample | Puzzle deriving (Eq, Ord, Show)

-- | default year for solve, getPuzzle, getSample functions
-- TODO: get year from environment, default to current year, put
-- that in a reader monad and thread it through the relevant 
-- functions
year :: Int
year = 22 

{-| Returns true if the second list starts with the first list -}
startsWith :: Eq a => [a] -> [a] -> Bool
startsWith target search
  | length search > length target || null search || null target = False
  | otherwise =
    let target' = take (length search) target in
    search == target'

{-| Given a filename gets the contents of that file as a string. 
    requires the environment variable @AOC2022_DATA@ to be set
    to the path (without trailing slash) where the data files
    are located. -}
getFile :: String -> IO String 
getFile file = do
  datapath <- getEnv "AOC2022_DATA"
  let fpath = datapath ++ "/" ++ file
  readFile fpath

{-| Returns all the (natural) numbers from a string -}
getNums :: String -> [Int]
getNums s =
  map read $ getAllTextMatches (s =~ ("[0-9]+" :: String))

tup2 :: [a] -> (a,a)
tup2 (x:y:_)= (x,y)
tup2 _ = error "must be at least 2 values in the list"

tup3 :: [a] -> (a,a,a)
tup3 (x:y:z:_) = (x,y,z)
tup3 _ = error "must be at least 3 values in the list"


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

{-| Zero pad an integer out to a certain number of digits -}
zpad :: Int -> Int -> String
zpad digits n =
  let sn = show n in
  replicate (digits - length sn) '0' ++ sn

{-| Internal solve, takes a boolean to determine if you're
    solving the sample (True) or the main puzzle (False) -}
solve' :: Show a => Int -> InputType -> Int -> (String -> a) -> IO ()
solve' yy it d fn = do
  str <- getTxt yy it d
  print (fn str)

{-| Gets the text for either the sample or puzzle data for
    the specified puzzle day. Files must follow the convention
    described in the documentation of @solve@ -}
getTxt :: Int -> InputType -> Int -> IO String
getTxt yy it d = do
  getFile $ 
    "Y20" ++ show yy ++ "/day" ++ zpad 2 d ++ 
    (if it == Sample then ".sample" else "") ++ ".txt"

{-| Solve the puzzle day indicated by the parameter. The puzzle data
    file must be in the directory indicated by system variable
    @AOC2022_DATA@ and follow the naming convention of 
    @day??.[sample.]txt@, where @??@ is the zero padded day number
    and [sample.] is included for the sample data but not for the
    puzzle data. -}
solve :: Show a => Int -> (String -> a) -> IO ()
solve = solve' year Puzzle

{-| Solve the sample data for the day indicated by the parameter. The
    solution function of signature @(String -> Int)@ is passed as
    a parameter (will have to generalize more later if they give
    us a puzzle with soltion type other than @Int@). See documentation f
    or @solve@ for information about puzzle file convention -}
sample :: Show a => Int -> (String -> a) -> IO ()
sample = solve' year Sample

{-| Gets the sample data for a particular puzzle day. Intended for
    use in the repl -}
getSample:: Int -> IO String
getSample = getTxt year Sample

{-| Gets the puzzle data for a particular puzzle day. Intended for
    use in the repl -}
getPuzzle:: Int -> IO String
getPuzzle = getTxt year Puzzle

