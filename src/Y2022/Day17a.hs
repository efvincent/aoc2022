{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2022.Day17a () where
import Util (getSample, getPuzzle)

samp :: IO String
samp = getSample 17
puzz :: IO String
puzz = getPuzzle 17

data Move = L | R deriving Show

type Moves = [Move]

type Board = [Int]    -- max Y (height) at each of the 7 column indexes

parse :: String -> Moves
parse = map (\c -> if c == '<' then L else R)

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

type Point = (Int,Int)
data Piece = P
    { _points :: [(Int,Int)]
    , _height :: Int }
    deriving (Show)

type Pieces = [Piece]

{-| limits indicate how far from the bottom of a starting rock the
    floor or highest rock is in any of the 7 columns wide of the board. 
    So for example, the starting rock on the starting board looks like:
    
    |oo####o|    
    |ooooooo|
    |ooooooo|
    |ooooooo|
    ---------

    and the limits are [3,3,3,3,3,3,3]
    with the first move of > in the sample, the next state is

    |ooo####|
    |ooooooo|
    |ooooooo|
    ---------

    the limits are now [2,2,2,2,2,2,2]    

-}
startingBoard :: Board
startingBoard = replicate 7 0

pieces :: [Piece]
pieces = 
    let ps =
          [ P [(0,0), (1,0), (2,0), (3,0)] 1
          , P [(1,2), (0,1), (1,1), (2,1), (1,0)] 3
          , P [(2,2), (2,1), (0,0), (1,0), (2,0)] 3
          , P [(0,3), (0,2), (0,1), (0,0)] 4
          , P [(0,1), (1,1), (0,0), (1,0)] 2] 
    in map (\(P pts h) -> P (map (\(x,y) -> (x + 2,y)) pts) h) ps

{-
algo:
    - (A) p = next piece @ starting location for board
    - (B) move = next move
    - if no next move, then DONE
    - p' = apply move p
    - if fits p' then
      - p = p'
    - else it doesn't fit going sideways
      - p = p 
    - p' = apply move_down p 
    - if fits p' then 
      - p = p' 
      - (B)
    - else it doesn't fit going down
      - update board p
      - (A)
    - (B)
-}

drop :: Piece -> Piece
drop (P ps h) = P (map (\(x,y) -> (x,y+1)) ps) h


