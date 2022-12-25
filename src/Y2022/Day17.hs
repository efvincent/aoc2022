{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2022.Day17 where

import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Util (getSample, getPuzzle, uncurry3)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.List (sortBy, intercalate)
import Data.List.Split (chunksOf)

samp :: IO String
samp = getSample 17
puzz :: IO String
puzz = getPuzzle 17

ij :: [Jet]
ij = parse ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

{-
    - board is 7 units wide
    - each rock begins so that:
      - its left edge 2 units from the left wall
      - its bottom edge is 3 units above the highest 
        rock in the wall (or the floor when there are no rocks)
    - pieces:


    0 1 2 3  
    -------
    # # # # | 0         (0,0), (1,0), (2,0), (3,0)

    0 1 2
    -----
      #   | 2           (1,2), (0,1), (1,1), (2,1), (1,0)
    # # # | 1
      #   | 0

    0 1 2
    -----
        # | 2           (2,2), (2,1), (0,0), (1,0), (2,0)
        # | 1
    # # # | 0

    0
    -
    # | 3               (0,3), (0,2), (0,1), (0,0)
    # | 2
    # | 1
    # | 0

    0 1
    ---
    # # | 1             (0,1), (1,1), (0,0), (1,0)
    # # | 0

-}

{-- Types & Consts-------------------------------------------------}

data Jet = L | R deriving (Show, Eq)

data Point = P
  { _x :: Int
  , _y :: Int } deriving (Ord,Show,Eq)

type Rock = Set Point

type Board = Set Point

initBoard :: Board
initBoard = S.fromList (map (`P` 0) [0..6])

down,left,right :: Point
down  = P 0 (-1)
left  = P (-1) 0
right = P 1 0

rocks :: [Rock]
rocks = cycle $ map (moveRock (P 2 0))
  [ S.fromList [P 0 0, P 1 0, P 2 0, P 3 0]
  , S.fromList [P 1 2, P 0 1, P 1 1, P 2 1, P 1 0]
  , S.fromList [P 2 2, P 2 1, P 0 0, P 1 0, P 2 0]
  , S.fromList [P 0 3, P 0 2, P 0 1, P 0 0]
  , S.fromList [P 0 1, P 1 1, P 0 0, P 1 0]]

{-- Solutions----------------------------------------------------}

testA :: IO ()
testA = do
  s <- getPuzzle 17
  let ans = sln17A s
  let msg =
        if ans == 3186 then "PASS"
        else "FAIL : Expecting 3186, got " ++ show ans
  putStrLn msg

sln17A :: String -> Int
sln17A s =
  let jets = parse s
  in maxY . sln 2022 $ jets

{-| calls @placeRock@ iteratively @n@ times and returns the state
    of the board with rocks placed-}
sln :: Int -> [Jet] -> Board
sln n jets =
  let (_,board',_) = iterate placeRock (rocks, initBoard, jets) !! n
  in board'

{-| pops the next rock off the stack, places it 3 units above the 
    top of the rock pile, then applies jets and drops until it 
    settles by calling @jetAndDrop@ -}
placeRock :: ([Rock],Board, [Jet]) -> ([Rock],Board,[Jet])
placeRock (rh:rt, board, jets) =
  let startingRock = moveRock (P 0 (maxY board + 4)) rh   -- move the rock to 3 above top of puzzle
      (boards', jets') = jetAndDrop startingRock board jets
  in (rt, boards', jets')

{-| applies a jet (within constraints) then a drop (again within
    constraints) and returns the new state of the board, and the
    remaining "jets" -}
jetAndDrop :: Rock -> Board -> [Jet] -> (Board, [Jet])
jetAndDrop origRock board (jh:jt) =
  let slid = jetRock jh origRock board
  in case dropRock slid board of
    Just droppedRock -> jetAndDrop droppedRock board jt
    Nothing ->
      (slid `S.union` board, jt)

{-- Helpers -----------------------------------------------------}

{-| jets a rock left or right and return the rock's new position,
    which may not have changed if there was something in the way
    or if the rock went out of bounds -}
jetRock :: Jet -> Rock -> Board -> Rock
jetRock j originalRock b =
  let movement     = if j == L then left else right
      slid         = moveRock movement originalRock
      inBoundsRock = fromMaybe originalRock (checkWall slid)
      okRock       = fromMaybe originalRock (checkBoard inBoundsRock b)
  in okRock

{-| drops a rock down one and returns Just the rock at the new
    position, or if not possible, returns Nothing -}
dropRock :: Rock -> Board -> Maybe Rock
dropRock originalRock b =
  let droppedRock = moveRock down originalRock
  in checkBoard droppedRock b

{-| Trims the board below where it may be possible to place any bricks.
    this is probably not the most effective way to do this, as I'm 
    breaking only on solid lines. -}

{-| Check if a rock is out of bounds wrt the walls. Return the rock
    if it is legal, otherwise return Nothing -}
checkWall :: Rock -> Maybe Rock
checkWall rock =
  let xs = S.map (\(P x _) -> x) rock
      minX = minimum xs
      maxX = maximum xs
  in if minX >= 0 && maxX <= 6 then Just rock else Nothing

{-| Check if a rock is in conflict with the rock pile. Return the
    rock if it is legal, otherwise return Nothing -}
checkBoard :: Rock -> Board -> Maybe Rock
checkBoard rock b =
  if  rock `S.disjoint` b then Just rock else Nothing

{-| produces an infinite array of jets -}
parse :: String -> [Jet]
parse = let pc '<' = L ; pc '>' = R in cycle . map pc

{-| prints the board to stdio in ascii as presented in AoC -}
pb :: Board -> IO ()
pb b =
  let mH = maxY b
      printCoords = [P x y | y <- [mH, (mH-1)..1], x <- [0..6]]
      coords      = chunksOf 7 . rowSorter $ printCoords
      output      = intercalate "\n" . mkBorders $ coords
  in putStrLn $ output ++ "\n+-------+\n maxY = " ++ show mH
  where
    mkBorders   = map ((\s -> '|':s ++ "|") . map pointChar)
    rowSorter   = sortBy $ \(P _ y) (P _ y') -> y' `compare` y
    pointChar c = if c `S.member` b then '█' else '.'

{-| returns the max/min @y@ of the rock pile -}
minY, maxY :: Board -> Int
maxY = predY (>)
minY = predY (<)

{-| use the predicate to find an element in the list -}
predY :: (Int -> Int -> Bool) -> Board -> Int
predY fn = foldl' (\acc (P _ y) -> if fn y acc then y else acc) 0

{-| translates a point by the x y of the other point -}
move :: Point -> Point -> Point
move (P x1 y1) (P x2 y2) = P (x1+x2) (y1+y2)

{-| moves a rock by translating all the points by the x y of 
    the point passed in the first parameter -}
moveRock :: Point -> Rock -> Rock
moveRock p = S.map (move p)

boardToMap :: Board -> [(Int, Int)]
boardToMap board =
  foldl' folder [] [minY board .. maxY board]
  where
    bitMapToInt :: [Int] -> Int
    bitMapToInt = foldl' (\acc x -> acc + 2^x) 0
    getRow b y = S.filter (\(P _ y') -> y == y') b
    rowToList = S.toList . S.map (\(P x _) -> x)
    row b = rowToList . getRow b
    folder acc y =
      let v = bitMapToInt (row board y)
      in (y, v):acc

{-
rTol s = S.toList . S.map (\(P x _) -> x) $ 
row b y = rTol . bfind b $ y
b' = foldl' (\acc y -> row b y : acc) [] [minY b .. maxY b]
-}
