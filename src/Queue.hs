{-|
Module      : Advent.Queue
Description : FIFO queue implementation
Copyright   : (c) Eric Mertens, 2018
                  Eric Vincent, 2022
License     : ISC
Maintainer  : info at efvincent dot com
Stability   : experimental

-}
{-# Language PatternSynonyms, ViewPatterns #-}
{-# Options_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Queue 
  ( Queue(Empty, (:<|))
  , (|>)
  , singleton
  , fromList
  , snoc
  , pop
  , appendList) 
where

import Data.Foldable (Foldable(..))

-- | FIFO Queue implementation
data Queue a = Queue [a] [a] !Int

{-# COMPLETE (:<|), Empty #-}

-- | Empty queue
pattern Empty :: Queue a
pattern Empty <- Queue [] _ _
  where
    Empty = Queue [] [] 0

-- | Pattern for 'pop'
pattern (:<|) :: a -> Queue a -> Queue a
pattern x :<| xs <- (pop -> Just (x, xs))

-- | Add an element to the end of a queue. See: 'snoc'
(|>) :: Queue a -> a -> Queue a
q |> x = snoc x q
{-# INLINE (|>) #-}

-- | Fold over elements in the order they would be returned by pop
instance Foldable Queue where
  null :: Queue a -> Bool
  null      (Queue l _ _) = null l

  length :: Queue a -> Int
  length    (Queue l r _) = length l + length r
  
  elem :: Eq a => a -> Queue a -> Bool
  elem x    (Queue l r _) = elem x l || elem x r
  
  sum :: Num a => Queue a -> a
  sum       (Queue l r _) = sum l + sum r
  
  foldMap :: Monoid m => (a -> m) -> Queue a -> m
  foldMap _ (Queue [] _ _) = mempty
  foldMap f (Queue (x:l) r 0) = f x <> rot l r
    where
      rot []     (y:_ ) = f y
      rot (x:xs) (y:ys) = f x <> rot xs ys <> f y
      rot _      _      = error "Assertion Failed: Queue invariant violated"
  foldMap f (Queue (x:l) r i) = f x <> foldMap f (Queue l r (i-1))


-- | Renders using 'fromList' syntax.
instance Show a => Show (Queue a) where
  showsPrec :: Int -> Queue a -> ShowS
  showsPrec p q
    = showParen (p >= 11)
    $ showString "fromList "
    . shows (toList q)

-- | Reads using the 'fromList' syntax
instance Read a => Read (Queue a) where
  readsPrec :: Int -> ReadS (Queue a)
  readsPrec prec
    = readParen (prec >= 11) $ \str ->
      do ("fromList", str) <- lex str
         (xs,         str) <- reads str
         return (fromList xs, str)

-- | Construct a queue from a single element.
singleton :: a -> Queue a
singleton x = Queue [x] [] 1

-- | Construct a queue from a list. The head of the list will
-- be the first element returned by 'pop'
fromList :: [a] -> Queue a
fromList xs = Queue xs [] (length xs)

-- | Append many items onto a queue. The items will pop from the queue
-- in the same order as they are in the given list.
appendList :: Queue a -> [a] -> Queue a
appendList = foldl' (|>)

-- | Remove an element from the front of a queue and a new queue
-- without that element.
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue (x:f) r s) = Just (x, exec f r s)
pop _                 = Nothing

-- | Add a new element to the end of a queue.
snoc :: a -> Queue a -> Queue a
snoc x (Queue f r s) = exec f (x:r) s

exec :: [a] -> [a] -> Int -> Queue a
exec f r 0    = fromList (rotate f r [])
exec f r i = Queue f r (i-1)

rotate :: [a] -> [a] -> [a] -> [a]
rotate []     (y:_ ) a = y : a
rotate (x:xs) (y:ys) a = x : rotate xs ys (y:a)
rotate _      _      _ = error "Assertion failed: Queue.rotate"
