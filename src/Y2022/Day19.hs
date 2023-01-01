{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-
Module      : Day19
Description : Advent of Code 2022 Day 19
Copyright   : (c) Eric Vincent, 2022
License     : ISC
Maintainer  : info at efvincent dot com
Stability   : experimental

** algorithm for maxGeodes for one blueprint (unoptimized) **

  - max_time = 24
  - max_geodes = 0
  - push initial state into the queue (no inventory, one ore bot)
  - (A) while there's some state to pop off the queue
    - for each bot, determine how long to make one
      - for each resource cost in the bot find the wait time to build it
        - find the wait time for each resource type cost
        - if resource cost < inventory for that type
          - wait time for this resource = 0
        - else if we have no bots that can make this resource
          - wait time is beyond max wait time (we'll never have this bot)
        - otherwise determine how long we'd have to wait for this resource
          - wait time = (cost - inventory for this type + #bots that make this -1) / #bots that make this
      - find the max wait time to build bot type in this state
      - wait time for this bot type in this state is the max wait time
    - end for each bot
    - new elapsed = elapsed + wait_time + 1 (the +1 is so the bot would have time do something)
    - if new elapsed > max time, skip this state, it's useless
      - continue (A)
    - gather ores with previously available bots
      - for each bot in your bot inventory, increase resource inventory by 1 
        for the type that it builds
    - increase the bot count for the bot we just built
    - push new state onto the queue with the new elapsed time (pref in the back)
    - continue (A)
  - after processing the queue
  - geodes for this blueprint = 
    number of geodes we have plus (# geode bots * (max_time - elapsed))    
-}

module Y2022.Day19 (sln19) where

import qualified Data.Vector as V
import Queue (Queue ((:<|)), (|>))
import qualified Queue as Q
import Data.Vector (Vector, (!))
import Util ( getNums, replaceAt, Parts (..) )
import Data.Foldable (foldl')

{-- Types _--------------------------------------------------------}

-- | Different types of bots & resources
data ResType = Ore | Clay | Obsidian | Geode
  deriving (Show, Eq, Ord, Enum)

{--| Resource amounts. Used for mineral counts, 
    robot counts, and robot build mineral costs -}
type Resources = Vector Int

-- | Costs for building robots in a particular blueprint
type Blueprint = Vector Resources

-- | all the blueprints in th puzzle
type Blueprints = Vector Blueprint

-- | State of bots and resources at a point in time
data State = State
  { _time :: Int
  , _bots :: Resources
  , _materials :: Resources }
  deriving (Show, Eq, Ord)

{-- Solutions ------------------------------------------------------}

-- | Returns the maximum number of Geodes a blueprint can produce in a certain amount of time
maxGeodes :: Int -> Blueprint -> Int
maxGeodes maxTime bp =
  -- start with no materials, and a single ore producing bot, and elapsed time is zero
  let startingQueue = Q.singleton State {_time=0, _bots=mkRes 1 0 0 0, _materials=mkRes 0 0 0 0}
  in processQueue 0 startingQueue 
  where
    -- since we can only make one bot per turn, it doesn't make sense to have more
    -- bots for a particular material type than the maximum cost across all bots
    -- for that material type. For example, if the bot that costs the most ore to 
    -- produce costs 10 ore, than we will never need more than 10 ore producing
    -- robots. Later we'll use this to ignore any potential plan that has us
    -- producing bots beyond this max bot limit
    maxBots = 
      let mc = V.fromList . map (\mIdx -> maximum . V.map (! mIdx) $ bp) $ [0..2]
      in V.snoc mc 10000 -- there is no limit to the number of geode bots we want to produce
    
    {-| pops state off the queue, and enqueues new states based on the popped state. Does so
        recursively until there are no more states to process in the queue. We can then examine
        the maximum that any of the states produced, and that's the maximum for this blueprint -}
    processQueue :: Int -> Queue State -> Int
    processQueue maxGeo = \case
      Q.Empty -> maxGeo
      curState@State{_time=elapsed,_bots=bots,_materials=mats} :<| queue ->
        let q' = foldl' (enquePlansFrom curState) queue [0..3]
            geodes = (mats ! 3) + (bots ! 3) * (maxTime - elapsed)
        in processQueue (max geodes maxGeo) q' 

    {-| given a state and bot (at @botIdx@), determine if that bot can be built, and if so,
        how much time it would take to build that bot. If we did build the bot, the elapsed
        time would increase by build time, we'd increase the bot count for that bot type,
        and material on hand would change, resulting in a new state, which we then enqueue -}
    enquePlansFrom :: State -> Queue State -> Int -> Queue State
    enquePlansFrom st@State{_time=elapsed,_bots=bots,_materials=mats} q botIdx
      | bots ! botIdx >= (maxBots ! botIdx) = q   -- don't build more than we need (see note in @maxGeodes@)
      | otherwise =
        case timeToBuildBot st botIdx of
          -- bot cannot be built, we won't be adding any new states to the queue
          Nothing -> q 
          -- bot can be built in @buildTime@
          Just buildTime ->
            -- if we build the bot, the new state would be ...
            let elapsed' = elapsed + buildTime + 1    -- elapsed time increased by build time
                curBot = bots ! botIdx               
                curBp  = bp   ! botIdx
                -- material would change for each material by the cost in that material for the bot we're building
                -- but it would go up by the number of bots we have building that material per turn
                mats'  = V.map (\mIdx -> (mats ! mIdx) + ((bots ! mIdx) * (buildTime + 1)) - (curBp ! mIdx)) $ V.fromList [0..3]
                -- and we of course would have one more bot of the type we built
                bots'  = V.fromList . replaceAt botIdx (curBot + 1) $ V.toList bots
            in q |> st { _time=elapsed', _bots=bots', _materials=mats'} -- enqueue this state to be investigated further later

    {-| calculate the time it takes to build a bot (indicated by @botIdx@) given the number of bots
        and resources we have in this current state -}
    timeToBuildBot :: State -> Int -> Maybe Int
    timeToBuildBot State{_bots=bots, _materials=mats} botIdx = do
      xs <- mapM timeForMaterial [0..3]
      pure $ maximum xs
      where
        recipe = bp ! botIdx
        timeForMaterial matIdx =
          let cost = recipe ! matIdx
              builders = bots ! matIdx
              inventory = mats ! matIdx
          in if cost <= inventory
            then Just 0
            else if builders == 0 then Nothing
            else let buildTime = (cost - inventory + builders - 1) `div` builders
                 in  if buildTime >= maxTime then Nothing else Just buildTime

sln19 :: Parts -> String -> Int
sln19 PartA = slnA
sln19 PartB = slnB

slnA :: String -> Int
slnA s =
  let p = parse s
      ans = V.zip (V.fromList [1::Int .. (length p)]) . V.map (maxGeodes 24) $ p
  in sum . V.map (uncurry (*)) $ ans

slnB :: String -> Int
slnB s = 
  let p = V.take 3 $ parse s
  in V.product . V.map (maxGeodes 32) $ p

{-- Helpers --------------------------------------------------------}

-- | makes a list of @Resources@
mkRes :: Int -> Int -> Int -> Int -> Resources
mkRes o c ob g =
  V.fromList [o, c, ob, g]

-- | parse puzzle 
parse :: String -> Blueprints
parse =
  V.fromList . map parseLine . lines
  where
    parseLine :: String -> Blueprint
    parseLine s =
      let [_,oCost,cCost,obsCost1,obsCost2,gCost1,gCost2] = getNums s
          bots =  [ mkRes oCost    0        0      0
                  , mkRes cCost    0        0      0
                  , mkRes obsCost1 obsCost2 0      0
                  , mkRes gCost1   0        gCost2 0]
      in V.fromList bots