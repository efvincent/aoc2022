module Grid 
  ( Grid
  , grid
  , boundGrid
  , toList
  , nbors4
  , nbors8
  , module A)
where

import Data.Array as A
  ( Ix(..)
  , Array
  , (!)
  , accum
  , accumArray
  , array
  , assocs
  , bounds
  , elems
  , indices
  , ixmap
  , listArray)
import Data.Bifunctor (first)

type Grid a = Array (Int,Int) a

grid :: [[a]] -> Grid a
grid g = A.array (boundGrid g) $ indexGrid g

boundGrid :: [[a]] -> ((Int,Int), (Int,Int))
boundGrid g = ((0,0), (pred . length . head $ g, pred . length $ g))

indexGrid :: [[c]] -> [((Int, Int), c)]
indexGrid = concat . zipWith (\y -> map (first (,y))) [0..] . map (zip [0..])

toList :: Grid a -> [[a]]
toList g = 
  [[g A.! (x,y) 
    | x <- [0..maxX]] 
    | let (maxX, maxY) = snd (A.bounds g), y <- [0..maxY]]

nbors4 :: Grid e -> (Int,Int) -> [(Int,Int)]
nbors4 g (x,y) =
  [ p | i <- [-1, 0, 1]
      , j <- [-1, 0, 1]
      , abs (i+j) == 1
      , let p = (x + i, y + j)
      , A.inRange (A.bounds g) p]

nbors8 :: Grid e -> (Int, Int) -> [(Int, Int)]
nbors8 g op@(x, y) =
  [ p | i <- [-1, 0, 1]
      , j <- [-1, 0, 1]
      , let p = (x + i, y + j)
      , p /= op
      , A.inRange (A.bounds g) p]