{-|
Module      : Search
Description : Search algorithms for Advent of Code
Copyright   : (c) Eric Mertens, 2018
                  Eric Vincent, 2022
License     : ISC
Maintainer  : info at efvincent dot com
Stability   : experimental

Currently implements depth first search with a few different overloads
for flexibility. See individually documented functions. 
-}
{-# LANGUAGE BangPatterns #-}
module Search 
  ( dfs
  , dfsN
  , dfsOn
  , dfsOnN
  , bfs
  , bfsN
  , bfsOn
  , bfsOnN)
where

import qualified Data.Set as S
import qualified Queue as Q
import Queue (Queue ((:<|)))

{-- Depth First Search ----------------------------------------------------}

-- | depth first search, first pattern is the next state function, second
--   is the starting state. The search is found when there's no next state.
dfs 
  :: Ord a      -- ^ states being searched must have @Ord@ instance
  => (a -> [a]) -- ^ calculate possible next states from current state
  -> a          -- ^ starting state
  -> [a]        -- ^ states visited in depth first order
dfs = dfsOn id

-- | depth first search with multiple possible start states
dfsN 
  :: Ord a      -- ^ states being searched required @Ord@  
  => (a -> [a]) -- ^ calculate possible next states from current state
  -> [a]        -- ^ possible starting states
  -> [a]        -- ^ states visited in depth first order
dfsN = dfsOnN id

-- | depth first search with state mapper function 
dfsOn 
  :: Ord r      -- ^ representation of the state must have an @Ord@ instance
  => (a -> r)   -- ^ function calculate a representation of the state
  -> (a -> [a]) -- ^ calculates possible next states from given state
  -> a          -- ^ starting state
  -> [a]        -- ^ states visited in depth first order
dfsOn
  rep   
  next  -- ^ successor function
  start -- ^ starting state
  = dfsOnN rep next [start]


{-| depth first search with state mapper function and multiple possible 
    starting states. The mapper function takes the collected item to a 
    representation of the collected item that will be used to 
    determine ordering and equality -}
dfsOnN 
  :: Ord r      -- ^ representation of the state must have an @Ord@ instance
  => (a -> r)   -- ^ function calculate a representation of the state
  -> (a -> [a]) -- ^ calculates possible next states from given state
  -> [a]        -- ^ possible starting states
  -> [a]        -- ^ states visited in depth first order
dfsOnN
  rep   -- ^ state characterization
  next  -- ^ successors function
  = loop S.empty 
    where
      loop !seen states = 
        case states of
          [] -> []
          x:xs 
            | S.member r seen -> loop seen xs
            | otherwise -> x:loop seen' (next x ++ xs)
            where
              r = rep x
              seen' = S.insert r seen

{-- Breadth First Search --------------------------------------------------}

bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id

bfsN :: Ord a => (a -> [a]) -> [a] -> [a]
bfsN = bfsOnN id

bfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
bfsOn rep next start = bfsOnN rep next [start]

bfsOnN :: Ord r => (a -> r) -> (a -> [a]) -> [a] -> [a]
bfsOnN rep next start =
  loop S.empty (Q.fromList start)
  where
    loop !seen = \case
      Q.Empty -> []
      x :<| q
        | S.member r seen -> loop seen q
        | otherwise -> x : loop seen' q'
        where
          r = rep x
          seen' = S.insert r seen
          q' = Q.appendList q (next x)

{-# INLINE bfs #-}
{-# INLINE bfsN #-}
{-# INLINE [0] bfsOn #-}
{-# INLINE [0] bfsOnN #-}