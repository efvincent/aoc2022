{-# OPTIONS_GHC -Wno-unused-imports #-}
module Y2022.Day18 where

import Util (getSample, getPuzzle)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.Foldable (minimumBy, maximumBy, toList)
import Algorithm.Search (bfs)
import Data.Maybe (fromMaybe, isNothing)

samp :: IO String
samp = getSample 18
puzz :: IO String
puzz = getPuzzle 18

data Cube = C Int Int Int deriving (Eq,Show,Ord)

type Cubes = Set Cube

cToTup :: Cube -> (Int,Int,Int)
cToTup (C x y z) = (x,y,z)

tupToCube :: (Int,Int,Int) -> Cube
tupToCube (x,y,z) = C x y z

parse :: String -> Cubes
parse = S.fromList . map (tupToCube . read . (\s -> "(" ++ s ++ ")")) . lines

nbors :: Cube -> [Cube]
nbors (C x y z) =
  [ C (x+1) y z, C (x-1) y z
  , C x (y+1) z, C x (y-1) z
  , C x y (z+1), C x y (z-1)]

slnA :: Cubes -> Int
slnA cubes =
  sum . map (sidesUnconnectedTo cubes) . toList $ cubes

sidesUnconnectedTo :: Cubes -> Cube -> Int
sidesUnconnectedTo others c =
  let cs = S.fromList . nbors $ c
  in length (cs \\ others)

bbox :: Cubes -> ((Int,Int),(Int,Int),(Int,Int))
bbox cubes =
  ( minMax (\(C x _ _) -> x)
  , minMax (\(C _ y _) -> y)
  , minMax (\(C _ _ z) -> z) )
  where
  minMax f =
    let comp c1 c2 = f c1 `compare` f c2
        mn = minimumBy comp cubes
        mx = maximumBy comp cubes
    in (f mn, f mx)

air :: Cubes -> Cubes
air cubes =
  let ((x1,x2), (y1,y2), (z1,z2)) = bbox cubes
      cs = S.fromList [C x y z | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]
  in cs \\ cubes

{-| given a set of cubes, return the set of cubes that are neighbors, but not
    cubes themselves. Think of theses as the cubes in contact with the input
    cubes that are not input cubes, or the air in contact with the mineral -}
contactAir :: Cubes -> Cubes
contactAir cubes =
  let nbs = concatMap nbors . toList $ cubes
      a = [c | c <- nbs, S.notMember c cubes]
  in S.fromList a

slnB cubes =
  --[() | c <- Set.toList cubes, n <- neigh c, Set.member    n air  ])
  -- let a = air2 cubes in
  -- length [() | c <- toList cubes, n <- nbors c, S.member n a] 
  -- length [() | c <- Set.toList cubes, n <- neigh c, Set.member    n air  ]
  let as = air3 cubes
      trapped = S.filter (\airCube -> sidesUnconnectedTo cubes airCube == 0) as
      surfaceArea = slnA cubes
  in surfaceArea - (length trapped * 6)

{-
findAir :: Set Coord3 -> Set Coord3
findAir cubes = Set.fromList (bfs step (hi + 1))
  where
    (lo, hi) = fromJust (boundingBox cubes)
    box      = (lo - 1, hi + 1)
    step c   = [n | n <- neigh c, inRange box n, Set.notMember n cubes]

  for each air that's a neighbor of a cube:
    see if bfs can find that cube from initial point
    if so, add it to "reachable" air
    otherwise don't
  total air is all nbors of cubes that are not in cubes
  after gathering reachable air, subtract total air from reachable air
-}
air3 :: Cubes -> Cubes
air3 cubes = cAir' -- S.fromList (fromMaybe [] $ bfs nextAir (const True) lo)
  where
    ((mnx,mxx), (mny,mxy), (mnz, mxz)) = bbox cubes
    (lo, hi) = (C (mnx-1) (mny-1) (mnz-1), C (mxx+1) (mxy+1) (mxz+1))
    nextAir c = [n | n <- nbors c, inRange lo hi n, S.notMember n cubes]
    cAir = contactAir cubes
    cAir' = S.filter (isNothing . bfs nextAir (lo ==)) cAir


inRange :: Cube -> Cube -> Cube -> Bool
inRange _lo@(C mnx mny mnz) _hi@(C mxx mxy mxz) _c@(C x y z) =
  x >= mnx && x <= mxx &&
  y >= mny && y <= mxy &&
  z >= mnz && z <= mxz



boundingBox :: Foldable f => f Cube -> Maybe (Cube, Cube)
boundingBox t =
  case toList t of
    []            -> Nothing
    C x y z : cs -> go x y z x y z cs
  where
    go lox loy loz hix hiy hiz [] =
        lo `seq` hi `seq` Just (lo, hi)
        where
          lo = C lox loy loz
          hi = C hix hiy hiz
    go lox loy loz hix hiy hiz (C x y z : cs) =
        go (min lox x) (min loy y) (min loz z) (max hix x) (max hiy y) (max hiz z) cs