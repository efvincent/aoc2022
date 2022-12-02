{- DAY02 : https://adventofcode.com/2022/day/2 -}
module Day02 (solve02)  where

import Util (getFile)

data Result = Win   | Lose | Draw     deriving Show
data Move   = Paper | Rock | Scissors deriving (Show, Eq, Ord)
type Game   = (Move, Move)

-- | Parse a single line from the input into a tuple of moves.
--   fst => their move, snd => our move
parseLine :: String -> (Move, Move)
parseLine [them,_,us] = 
  (decode them, decode us)
  where
    decode x | x `elem` ['A','X'] = Rock
    decode x | x `elem` ['B','Y'] = Paper
    decode x | x `elem` ['C','Z'] = Scissors 
    decode c = error $ "invalid input: " ++ [c]
parseLine s = error $ "invalid line: " ++ s

-- | Get the solution for either part. When @partA@ is @True@,
--   we return the solution for part A of the puzzle. 
--   otherwise we return the solution for part B
sln :: Bool -> String -> Int
sln partA s = 
  let fn = if partA then id else decodeGame in
  sum . map (uncurry getPoints . fn . parseLine) . lines $ s 

-- | get the decoded game from the raw game input according to
--   the rules of part B
decodeGame :: Game -> Game
decodeGame (a,b) = (a, decodeMove (getEncodedResult b) a)

-- | get the puzzle points of a game
getPoints :: Move -> Move -> Int
getPoints Rock     Rock     = 3 + 1   -- draws
getPoints Paper    Paper    = 3 + 2
getPoints Scissors Scissors = 3 + 3
getPoints Rock     Paper    = 6 + 2   -- wins
getPoints Paper    Scissors = 6 + 3
getPoints Scissors Rock     = 6 + 1
getPoints Rock     Scissors = 3       -- losses
getPoints Paper    Rock     = 1
getPoints Scissors Paper    = 2

-- | get the encoded result
getEncodedResult :: Move -> Result
getEncodedResult = \case 
  Rock     -> Lose
  Paper    -> Draw
  Scissors -> Win

-- | get the move we should make to get a result given the other move
decodeMove :: Result -> Move -> Move
decodeMove Draw m        = m
decodeMove Win  Rock     = Paper
decodeMove Win  Paper    = Scissors
decodeMove Win  Scissors = Rock
decodeMove Lose Rock     = Scissors
decodeMove Lose Paper    = Rock
decodeMove Lose Scissors = Paper

{-| solve the puzzle for either part based on parameters -}
solve02 :: Bool -> IO ()
solve02 forPartA = do
  raw <- getFile "day02.txt"
  print (sln forPartA raw)