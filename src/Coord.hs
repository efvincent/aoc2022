{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-|
Module      : Coord
Description : 2D Coordinate library for Advent of Code use
Copyright   : (c) Eric Mertens, 2018
                  Eric Vincent, 2022
License     : ISC
Maintainer  : info at efvincent dot com
Stability   : experimental

2d coordinate type and functions common in AoC problems. Like
typical screen coordinates, origin is at (y,x), x grows right,
and y grows down.

Note that the rows come first, then the columns. This allows for
row-major coordinate indexing which is normal when using
array (or vector) storage

-}
module Coord 
  ( Coord (..)
  , cRow
  , cCol
  , cX
  , cY
  , above
  , below
  , right
  , left
  , turnRight
  , turnLeft
  , turnAround
  , scaleCoord
  , manhattan
  , norm1
  , origin
  , north
  , south
  , east
  , west
  , neighbors
  , diags
  , allNeighbors)
where


import GHC.Generics (Generic)
import Data.Data (Data)
import GHC.Ix (Ix, indexError)
import GHC.Arr (Ix(..))
data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq, Generic, Data)

-- | Get the @y@ coordinate (row)
cRow :: Coord -> Int
cRow (C y _) = y

-- | Get the @x@ coordinate (column)
cCol :: Coord -> Int
cCol (C _ y) = y

-- | Get the @x@ coordinate (column)
cX :: Coord -> Int
cX = cCol

-- | Get the @y@ coordinate (row)
cY :: Coord -> Int
cY = cRow

-- | return the coordinate just above the argument
above :: Coord -> Coord
above (C y x) = C (y-1) x

-- | return the coordinate just below the argument
below :: Coord -> Coord
below (C y x) = C (y+1) x

-- | return the coordinate just left of the argument
left :: Coord -> Coord
left (C y x) = C y (x-1)

-- | return the coordinate just right of the argument
right :: Coord -> Coord
right (C y x) = C y (x+1)

-- | turn unit vector 90degrees left
turnLeft :: Coord -> Coord
turnLeft (C y x) = C (negate x) y

-- | turn unit vector 90degrees right
turnRight :: Coord -> Coord
turnRight (C y x) = C x (negate y)

-- | reverse direction of unit vector
turnAround :: Coord -> Coord
turnAround (C y x) = C (negate y) (negate x)

-- | compute the manhattan distance between 2 points
manhattan :: Coord -> Coord -> Int
manhattan  a b = norm1 (a - b)

-- | Coordinate at the origin
origin :: Coord
origin = C 0 0

-- | Unit vector pointing up
north :: Coord
north = C (-1) 0

-- | Unit vector pointing right
east :: Coord
east = C 0 1

-- | Unit vector pointing down
south :: Coord
south = C 1 0

-- | Unit vector pointing left
west :: Coord
west = C 0 (-1)

-- | returns the neighbors of the coordinate, not including diagonals
neighbors :: Coord -> [Coord]
neighbors (C y x) =
  [ C (y+1) x, C (y-1) x
  , C y (x+1), C y (x-1)]  

-- | returns the diagonals of the coordinate
diags :: Coord -> [Coord]
diags (C y x) =
  [ C (y+1) (x+1), C (y-1) (x-1)
  , C (y-1) (x+1), C (y+1) (x-1)]  

-- | returns the neighbors of the coordinate, including diagonals
allNeighbors :: Coord -> [Coord]
allNeighbors c = neighbors c ++ diags c

{-| compute 1-norm between two coordinates (sum magnitudes).
    this is the same as the manhattan distance from the origin
    to the coordinate. -}
norm1 :: Coord -> Int
norm1 (C y x) = abs y + abs x

-- | Apply a function to the y and x coordinate
mapCoord :: (Int -> Int) -> Coord -> Coord
mapCoord f (C y x) = C (f y) (f x)

-- | Use a function pairwise on the @x@ and @y@ coordinates of the two arguments
zipCoord :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
zipCoord f (C y1 x1) (C y2 x2) = C (f y1 y2) (f x1 x2)

-- | scale a unit vector
scaleCoord :: Int -> Coord -> Coord
scaleCoord n = mapCoord (n *)

-- | Vector arithmetic
instance Num Coord where
  (+) :: Coord -> Coord -> Coord
  (+) = zipCoord (+)
  
  (-) :: Coord -> Coord -> Coord
  (-) = zipCoord (-)
  
  (*) :: Coord -> Coord -> Coord
  (*) = zipCoord (*)
  
  negate :: Coord -> Coord
  negate = mapCoord negate
  
  abs :: Coord -> Coord
  abs = mapCoord abs
  
  signum :: Coord -> Coord
  signum = mapCoord signum

  fromInteger :: Integer -> Coord
  fromInteger = (\i -> C i i) . fromInteger
  
  {-# INLINE (+)          #-}
  {-# INLINE (-)          #-}
  {-# INLINE (*)          #-}
  {-# INLINE signum       #-}
  {-# INLINE fromInteger  #-}
  {-# INLINE negate       #-}
  {-# INLINE abs          #-}

-- | Indexing
instance Ix Coord where
  unsafeIndex :: (Coord, Coord) -> Coord -> Int
  unsafeIndex (C lorow locol, C hirow hicol) (C row col) =
    unsafeIndex (lorow,hirow) row * unsafeRangeSize (locol,hicol) + unsafeIndex (locol,hicol) col

  index :: (Coord, Coord) -> Coord -> Int
  index b i
    | inRange b i = unsafeIndex b i
    | otherwise   = indexError b i "Coord"

  inRange :: (Coord, Coord) -> Coord -> Bool
  inRange (C lorow locol, C hirow hicol) (C row col) =
    inRange (lorow,hirow) row && inRange (locol,hicol) col

  range :: (Coord, Coord) -> [Coord]
  range (C lorow locol, C hirow hicol) =
    [C row col | row <- [lorow..hirow], col <- [locol..hicol]]

  unsafeRangeSize :: (Coord, Coord) -> Int
  unsafeRangeSize (C lorow locol, C hirow hicol) =
    (hirow - lorow + 1) * (hicol - locol + 1)
  
  {-# INLINE unsafeRangeSize  #-}
  {-# INLINE unsafeIndex      #-}
  {-# INLINE index            #-}
  {-# INLINE inRange          #-}
  {-# INLINE range            #-}