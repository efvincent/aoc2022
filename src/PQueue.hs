{-|
Module      : Advent.PQueue
Description : Int-priority min queue
Copyright   : (c) Eric Mertens, 2018
              (c) Eric Vincent, 2022
License     : ISC
Maintainer  : info at efvincent dot com

Priority queue with 'Int' priorities returning smallest priority first
-}

{-# Language PatternSynonyms, ViewPatterns, DeriveTraversable #-}
module PQueue 
  ( PQueue(Empty, (:<|))
  , singleton
  , fromList
  , insert
  , PQueue.null
  , view
  , viewWithPriority)
where

import qualified Data.IntMap.Strict as IM

{-| Priority queue (lower priorities first). No guarentees are made regarding
    return order of values at the same priority -}
newtype PQueue a = PQ (IM.IntMap [a])
  deriving (Functor, Foldable, Traversable)

-- | Show a @PQueue@ using @fromList@
-- >>> show (singleton 1 'a')
-- "fromList [(1,'a')]"
instance Show a => Show (PQueue a) where
  showsPrec :: Int -> PQueue a -> ShowS
  showsPrec prec (PQ q) =
    showParen (prec >= 11) $ showString "fromList "
    . shows [(p,v) | (p, vs) <- IM.toList q, v <- vs]

-- | Read a priority queue as a list of tuples
-- >>> read "fromList [(1,'a'), (3, 'b')]"
-- Prelude.read: no parse
instance Read a => Read (PQueue a) where
  readsPrec :: Int -> ReadS (PQueue a)
  readsPrec prec =
    readParen (prec >= 11) $ 
    \str -> do
      ("fromList", str') <- lex str
      (xs        , str'') <- reads str'
      pure (fromList xs, str'')

{-# Complete Empty, (:<|) #-}

-- | Empty priority queue
pattern Empty :: PQueue a
pattern Empty <- (PQueue.null -> True)
  where 
    Empty = PQ IM.empty

-- | pattern for extracting an element with minimum priority. Also see @view@.
pattern (:<|) :: a -> PQueue a -> PQueue a
pattern v :<| q <- (view -> Just (v,q))

-- | test if a queue has no elements
null :: PQueue a -> Bool
null (PQ q) = IM.null q

-- | construct a priority queue from a single priority and value
singleton :: Int -> a -> PQueue a
singleton p v = PQ (IM.singleton p [v])

-- | insert a new value into the queue at a given priority
insert :: Int -> a -> PQueue a -> PQueue a
insert k v (PQ q) = PQ (IM.alter aux k q)
  where
    aux Nothing = Just [v]
    aux (Just vs) = Just (v:vs)

{-| Match the lowest priority entry in a queue returning the corresponding
    value and queue without that entry. See also @(:<|)@ -}
view :: PQueue a -> Maybe (a, PQueue a)
view (PQ q) = do 
  ((k,xs),q') <- IM.minViewWithKey q
  case xs of 
    [] -> error "Assertion failed: Malformed queue"
    [x] -> Just (x, PQ q')
    x:xs' -> let q'' = PQ (IM.insert k xs' q') in q'' `seq` Just (x, q'')

{-| Match the lowest priority entry in a queue returning the corresponding
    value and queue without that entry. -}
viewWithPriority :: PQueue a -> Maybe (Int, a, PQueue a)
viewWithPriority (PQ q) = do
  ((k,xs),q') <- IM.minViewWithKey q
  case xs of 
    [] -> error "Assertion failed: Malformed queue"
    [x] -> Just (k, x, PQ q')
    x:xs' -> let q'' = PQ (IM.insert k xs' q') in q'' `seq` Just (k, x, q'')

-- | Construct a priority queue from a list of priorities and values
fromList :: [(Int,a)] -> PQueue a
fromList xs = PQ (IM.fromListWith (++) [(p,[v]) | (p,v) <- xs])
