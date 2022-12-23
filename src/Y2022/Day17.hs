{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2022.Day17 where

import Data.Set (Set)
import qualified Data.Set as S
import Util (getSample, getPuzzle, uncurry3)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.List (sortBy, intercalate)
import Data.List.Split (chunksOf)
import Debug.Trace (trace)

samp :: IO String
samp = getSample 17
puzz :: IO String
puzz = getPuzzle 17

ij :: [Jet]
ij = parse ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

{-- Types & Consts-------------------------------------------------}

data Jet = L | R deriving (Show, Eq)

data Point = P Int Int
  deriving (Ord,Show,Eq)

data Rock = Rock (Set Point) Int
  deriving Show

type Board = Set Point

initBoard :: Board
initBoard = S.fromList (map (`P` 0) [0..6])

down,left,right :: Point
down  = P 0 (-1)
left  = P (-1) 0
right = P 1 0


rocks :: [Rock]
rocks = cycle $ map (moveRock (P 2 0))
  [ Rock (S.fromList [P 0 0, P 1 0, P 2 0, P 3 0]       ) 1
  , Rock (S.fromList [P 1 2, P 0 1, P 1 1, P 2 1, P 1 0]) 3
  , Rock (S.fromList [P 2 2, P 2 1, P 0 0, P 1 0, P 2 0]) 3
  , Rock (S.fromList [P 0 3, P 0 2, P 0 1, P 0 0]       ) 4
  , Rock (S.fromList [P 0 1, P 1 1, P 0 0, P 1 0]       ) 2]


{-- Solutions----------------------------------------------------}

sln17 :: Int -> [Jet] -> Board
sln17 n jets =
  let (_,board',_) = iterate placeRock (rocks, initBoard, jets) !! n
  in board'

placeRock :: ([Rock],Board, [Jet]) -> ([Rock],Board,[Jet])
placeRock (rh@(Rock _ height):rt, board, jets) =
  let r = moveRock (P 0 (maxH board + height + 3)) rh
      (board', jets') = go r board jets
  in (rt, board', jets')
  -- where
go :: Rock -> Board -> [Jet] -> (Board, [Jet])
go r@(Rock _ _) b (jh:jt) =
  let r'@(Rock pts _) = jetRock jh r b
  in case dropRock r' b of
    Just r'' -> go r'' b jt
    Nothing -> (S.union b pts, jt)

{-- Helpers -----------------------------------------------------}

{-| jets a rock left or right and return the rock's new position,
    which may not have changed if there was something in the way
    or if the rock went out of bounds -}
jetRock :: Jet -> Rock -> Board -> Rock
jetRock j r b =
  let f = if j == L then left else right
      r' = moveRock f r
  in fromMaybe r (checkRock r' b)

{-| returns true if rock does not conflict with the board -}
checkRock :: Rock -> Board -> Maybe Rock
checkRock r@(Rock pts _) b =
  let xs = S.map (\(P x _) -> x) pts
      minX = minimum xs
      maxX = maximum xs
      overlap = S.intersection pts b
      overlap' = if null overlap then overlap else (trace ("intersection:" ++ show overlap ++ " pts:" ++ show pts ++ " board:" ++ show b) overlap)
  in if minX >= 0 && maxX <= 6 && null overlap'
     then Just r 
     else Nothing

{-| slides a rock in the direction of the jet, so long as it's
    not blocked by one of the pieces on the board -}
slide :: Jet -> Rock -> Board -> Rock
slide j r b =
  let r' = jetRock j r b
  in fromMaybe r $ checkRock r' b

{-| drops a rock down one and returns Just the rock at the new
    position, or if not possible, returns Nothing -}
dropRock :: Rock -> Board -> Maybe Rock
dropRock r b =
  let r' = moveRock down r
  in checkRock r' b

{-| produces an infinite array of jets -}
parse :: String -> [Jet]
parse = let pc '<' = L ; pc '>' = R in cycle . map pc

pb :: Board -> IO ()
pb b =
  let mH = maxH b
      coords = chunksOf 7 . sortBy (\(P _ y) (P _ y') -> y' `compare` y) $ [P x y | y <- [mH, (mH-1)..1], x <- [0..6]]
      strs = intercalate "\n" $ map ((\s -> '|':s ++ "|") . map mcoord) coords
  in putStrLn $ strs ++ "\n+-------+"
  where
    mcoord :: Point -> Char
    mcoord c =
      if c `S.member` b then '#' else '.'

maxH :: Board -> Int
maxH = foldl' (\acc (P _ y) -> if acc < y then y else acc) 0

move :: Point -> Point -> Point
move (P x1 y1) (P x2 y2) = P (x1+x2) (y1+y2)

moveRock :: Point -> Rock -> Rock
moveRock p (Rock pts n) = Rock (S.map (move p) pts) n
