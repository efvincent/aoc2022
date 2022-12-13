{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Fifo
  ( Fifo
  , push
  , pop
  , peek
  , singleton
  , fromList ) where


newtype Fifo a = Fifo ([a], [a]) deriving (Show)

push :: a -> Fifo a -> Fifo a
push x (Fifo ([], s2)) = Fifo (reverse (x:s2), [])
push x (Fifo (s1, s2)) = Fifo (s1, x:s2)

pop :: Fifo a -> (Fifo a, Maybe a)
pop q@(Fifo ([], []))  = (q,Nothing)
pop   (Fifo ([], s2))  = let (h:t) = reverse s2 in (Fifo (t,[]), Just h)
pop   (Fifo (h:t, s2)) = (Fifo (t, s2), Just h)

peek :: Fifo a -> Maybe a
peek (Fifo ([], [])) = Nothing
peek (Fifo ([], s2)) = Just . last $ s2
peek (Fifo (h:_, _)) = Just h

singleton :: a -> Fifo a
singleton x = Fifo ([x], [])

fromList :: [a] -> Fifo a
fromList xs = Fifo (xs, [])