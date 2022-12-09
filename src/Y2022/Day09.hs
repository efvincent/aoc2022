{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Y2022.Day09 where

import qualified Data.Set as S
import Util (getSample, getPuzzle)
import Data.List.Split (splitOn)
import Data.List (foldl')

samp :: IO String
samp = getSample 9
puzz :: IO String
puzz = getPuzzle 9

data Dir = U | D | L | R deriving (Show, Eq, Ord, Read)

type Instr = (Dir, Int)
type Loc   = (Int,Int)
type Seen  = S.Set Loc

parse :: String -> [Instr]
parse = map (\s -> let [i,n] = splitOn " " s in (read i, read n)) . lines

move :: Dir -> Loc -> Loc
move U (x,y) = (x,y-1)
move D (x,y) = (x,y+1)
move L (x,y) = (x-1,y)
move R (x,y) = (x+1,y)

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

sln09A :: String -> Int
sln09A s =
  let ins       = parse s
      h         = (0,0)
      (_,_,ans) = foldl' go (h, h, S.singleton h) ins
  in length ans
  where
    go :: (Loc, Loc, Seen) -> Instr -> (Loc,Loc,Seen)
    go (ch, ct, seen) (_, 0) = (ch,ct,seen)
    go (ch, ct, seen) (d, dist) =
      let
        ch' = move d ch
        ct' = chase ch' ct
      in go (ch', ct', S.insert ct' seen) (d, dist - 1)

sln09B :: String -> Int
sln09B s =
  let 
    ins        = parse s
    h          = (0,0)
    body       = replicate 9 h
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

    moveBody :: [Loc] -> [Loc]
    moveBody [b9] = [b9]
    moveBody (b0:b1:rest) =
      let
        b1' = chase b0 b1
      in b0 : moveBody (b1':rest) 
