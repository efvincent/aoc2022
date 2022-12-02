{- DAY02 : https://adventofcode.com/2022/day/2 -}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day02 (solve02) where
import Util (getFile)

data Result = Win   | Lose | Draw     deriving Show
data Move   = Paper | Rock | Scissors deriving (Show, Eq)
type Game   = (Move, Move)

parseLine :: String -> (Move, Move)
parseLine [them,_,us] = 
  (decode them, decode us)
  where
    decode x | x `elem` ['A','X'] = Rock
    decode x | x `elem` ['B','Y'] = Paper
    decode x | x `elem` ['C','Z'] = Scissors 
    decode c = error $ "invalid input: " ++ [c]

-- | Get the solution for either part based on parameter
sln :: Bool -> String -> Int
sln partA s = 
  let fn = if partA then id else decodeGame in
  sum . map (gamePts . fn . parseLine) . lines $ s 

-- | for part B, get the decoded game from the raw game input
decodeGame :: Game -> Game
decodeGame (a,b) = (a, getPlay (getRes b) a)

-- | determine number of points for a move
movePts :: Move -> Int
movePts Rock     = 1
movePts Paper    = 2
movePts Scissors = 3

resPts :: Result -> Int
resPts Win  = 6
resPts Draw = 3
resPts Lose = 0

-- | get the points for a game
gamePts :: Game -> Int
gamePts (t, u) = resPts (result t u) + movePts u

-- | get the result of a game
result :: Move -> Move -> Result
result a b | a == b      = Draw
result Rock     Paper    = Win
result Paper    Scissors = Win
result Scissors Rock     = Win
result Rock     Scissors = Lose
result Paper    Rock     = Lose
result Scissors Paper    = Lose

-- | get the encoded result
getRes :: Move -> Result
getRes Rock     = Lose
getRes Paper    = Draw
getRes Scissors = Win

-- | get the move we should make to get a result given the other move
getPlay :: Result -> Move -> Move
getPlay Draw m        = m
getPlay Win Rock      = Paper
getPlay Win Paper     = Scissors
getPlay Win Scissors  = Rock
getPlay Lose Rock     = Scissors
getPlay Lose Paper    = Rock
getPlay Lose Scissors = Paper

{-| solve the puzzle part A -}
solve02 :: Bool -> IO ()
solve02 forPartA = do
  raw <- getFile "day02.txt"
  print (sln forPartA raw)