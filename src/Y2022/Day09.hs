{- DAY09 : https://adventofcode.com/2022/day/9 -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Y2022.Day09 (sln09A, sln09B) where

import qualified Data.Set as S
import Data.List.Split (splitOn)
import Util (Parts (..))

{-- Types --------------------------------------------------------}

-- | direction part of the instruction
data Dir = U | D | L | R deriving (Show, Eq, Ord, Read)

-- | instructions are a direction, and a magnitude (distance)
type Instr = (Dir, Int)

-- | location on a grid
type Loc   = (Int,Int)

-- | Set of seen locations
type Seen  = S.Set Loc
        
{-- Solutions ----------------------------------------------------- 
    Standard wrappers that can be leveraged by @Util.solve@ 
-------------------------------------------------------------------}

sln09A :: String -> Int
sln09A = sln PartA 

sln09B :: String -> Int
sln09B = sln PartB

sln :: Parts -> String -> Int
sln p s =
  let 
    ins        = parse s
    h          = (0,0)
    body       = replicate (if p == PartA then 1 else 9) h
    (_,_,seen) = loop (h,body,S.singleton h) ins
  in length seen
  where

    {-| loops through instructions -}
    loop :: (Loc, [Loc], Seen) -> [Instr] -> (Loc, [Loc], Seen)
    loop ans [] = ans
    loop (h,body,seen) (i:instrs) =
      let 
        (h', body', seen') = moveHead h body i seen
      in loop (h', body', seen') instrs 

    {-| for each instruction, moves the entire rope, then 
        captures the last position in the seen set -}
    moveHead :: Loc -> [Loc] -> Instr -> Seen -> (Loc,[Loc],Seen)
    moveHead h body      (_,0)    seen = (h,body,seen) 
    moveHead h (b0:rest) (d,dist) seen =
      let
        h'    = move  d  h
        b0'   = chase h' b0
        body' = moveBody (b0':rest)
        seen' = S.insert (last body') seen
      in moveHead h' body' (d,dist-1) seen'

    {-| moves the body, assuming the head of the rope has been moved.
        In part A, this is a noOp, in part B, this makes the rest of
        the rope chase segment 1 that was moved in @moveHead@ -}
    moveBody :: [Loc] -> [Loc]
    moveBody [b9] = [b9]
    moveBody (b0:b1:rest) =
      let
        b1' = chase b0 b1
      in b0 : moveBody (b1':rest) 

{-- Helpers -------------------------------------------------------}

-- | chase the first location with the second location - in other words,
--   the second location passed is the one that's doing the chasing
chase :: Loc -> Loc -> Loc
chase (xh,yh) (xt,yt) =
  let
    (dx,dy) = (xh - xt, yh - yt)
    (mx,my) =
      if abs dx + abs dy > 2
      then
        -- diagonal mode move both 1 spot
        let mx' = if dx < 0 then -1 else 1
            my' = if dy < 0 then -1 else 1
        in (mx', my')
       else
        -- in line or zero mode
        let mx' = if dx > 1 then 1 else if dx < -1 then -1 else 0
            my' = if dy > 1 then 1 else if dy < -1 then -1 else 0
        in  (mx', my')
  in (xt+mx,yt+my)

-- | parse input into a string
parse :: String -> [Instr]
parse = map (\s -> let [i,n] = splitOn " " s in (read i, read n)) . lines

-- | Move a point in the specified direction one step
move :: Dir -> Loc -> Loc
move U (x,y) = (x,y-1)
move D (x,y) = (x,y+1)
move L (x,y) = (x-1,y)
move R (x,y) = (x+1,y)
