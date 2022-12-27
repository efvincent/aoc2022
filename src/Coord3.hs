{-|
Module      : Coord3
Description : 3D Coordinate library for Advent of Code use
Copyright   : (c) Eric Mertens, 2018
                  Eric Vincent, 2022
License     : ISC
Maintainer  : info at efvincent dot com
Stability   : experimental

Many AoC puzzles use discreet 3d coordinates, this library provides
the most common functions found useful in manipulating 3d coords
in the AoC puzzles. 
-}

{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Coord3 
  ( Coord3(..)
  , boundingBox
  , manhattan
  , mapCoord
  , zipCoord
  , origin
  , neighbors
  , from3Tuple
  , to3Tuple)
where

import Data.Data (Data)
import GHC.Generics (Generic)
import Data.Foldable (toList)
import GHC.Ix (Ix(..))

-- | Three dimensional coordinates x y z
data Coord3 = C3
  !Int -- ^ the @x@ coordinate
  !Int -- ^ the @y@ coordinate
  !Int -- ^ the @z@ coordinate
  deriving (Eq, Ord, Show, Generic, Data)

-- | alias for @C3 0, 0, 0@
origin :: Coord3
origin = C3 0 0 0

-- | manhattan distance between 2 3-dimensional coordinates
manhattan
  :: Coord3   -- ^ first coordinate to measure 
  -> Coord3   -- ^ second coordinate to measure
  -> Int      -- ^ distance between two coordinates
manhattan (C3 x1 y1 z1) (C3 x2 y2 z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

-- | returns the neighbors of the coordinate, not including diagonals
neighbors :: Coord3 -> [Coord3]
neighbors (C3 x y z) =
  [ C3 (x+1) y z, C3 (x-1) y z
  , C3 x (y+1) z, C3 x (y-1) z
  , C3 x y (z+1), C3 x y (z-1)] 

-- | converts a 3-tuple of Ints to a Coord3
from3Tuple :: (Int, Int, Int) -> Coord3
from3Tuple (x,y,z) = C3 x y z

-- | converts a Coord3 to a 3-tuple
to3Tuple :: Coord3 -> (Int, Int, Int)
to3Tuple (C3 x y z) = (x,y,z)

-- | calculate the bounding box of a @Foldable@ of coordinates
boundingBox
  :: Foldable f             -- ^ the collection must be @Foldable@ so it can be itereated
  => f Coord3               -- ^ the set of points for which to calculate the bounding box
  -> Maybe (Coord3, Coord3) -- ^ if input is valid, the bounding box of the set of coordinates
boundingBox t =
  case toList t of
    [] -> Nothing
    C3 x y z : cs -> go x y z x y z cs
  where
    go mnx mny mnz mxx mxy mxz [] =
      mn `seq` mx `seq` Just (mn, mx)
      where
        mn = C3 mnx mny mnz
        mx = C3 mxx mxy mxz
    go mnx mny mnz mxx mxy mxz (C3 x y z : cs) =
      go (min mnx x) (min mny y) (min mnz z)
         (max mxx x) (max mxy y) (max mxz z) cs

-- | Apply a function to the @x@, @y@, and @z@ coordinates of the arguments
mapCoord
  :: (Int -> Int) -- ^ the function to be applied to each dimension
  -> Coord3       -- ^ the coordinate to which the map will be applied
  -> Coord3       -- ^ the result of applying the function to the input coordinate
mapCoord f (C3 x y z) = C3 (f x) (f y) (f z)

-- | Use a function pair wise on the @x@, @y@, and @z@ coordinates of the arguments
zipCoord
  :: (Int -> Int -> Int)  -- ^ the function to apply pairwise to each of the two input coordinates 
  -> Coord3               -- ^ the first coordinate to zip
  -> Coord3               -- ^ the second coordinate to zip
  -> Coord3               -- ^ the result of applying the function pairwise to the 
                          --   two input coordinates
zipCoord f (C3 x1 y1 z1) (C3 x2 y2 z2) = C3 (f x1 x2) (f y1 y2) (f z1 z2)

-- | Vector arithmetic
instance Num Coord3 where
  (+) :: Coord3 -> Coord3 -> Coord3
  (+) = zipCoord (+)

  (-) :: Coord3 -> Coord3 -> Coord3
  (-) = zipCoord (-)

  (*) :: Coord3 -> Coord3 -> Coord3
  (*) = zipCoord (*)

  negate :: Coord3 -> Coord3
  negate = mapCoord negate

  abs :: Coord3 -> Coord3
  abs = mapCoord abs

  signum :: Coord3 -> Coord3
  signum = mapCoord signum

  fromInteger :: Integer -> Coord3
  fromInteger = (\i -> C3 i i i) . fromInteger

  {-# INLINE (+)          #-}
  {-# INLINE (-)          #-}
  {-# INLINE (*)          #-}
  {-# INLINE negate       #-}
  {-# INLINE abs          #-}
  {-# INLINE signum       #-}
  {-# INLINE fromInteger  #-}

-- | Indexing 3d coordinates
instance Ix Coord3 where
  range :: (Coord3, Coord3) -> [Coord3]
  range (C3 mnx mny mnz, C3 mxx mxy mxz) =
    [C3 x y z | x <- range(mnx, mxx), y <- range(mny, mxy), z <- range(mnz, mxz)]
  
  unsafeIndex :: (Coord3, Coord3) -> Coord3 -> Int
  unsafeIndex (C3 mnx mny mnz, C3 mxx mxy mxz) (C3 x y z) =
    unsafeIndex (mnz, mxz) z + unsafeRangeSize (mnz, mxz) * (
    unsafeIndex (mny, mxy) y + unsafeRangeSize (mny, mxy) *
    unsafeIndex (mnx, mxx) x)

  inRange :: (Coord3, Coord3) -> Coord3 -> Bool
  inRange (C3 mnx mny mnz, C3 mxx mxy mxz) (C3 x y z) =
    inRange (mnx, mxx) x && inRange (mny, mxy) y && inRange (mnz, mxz) z  