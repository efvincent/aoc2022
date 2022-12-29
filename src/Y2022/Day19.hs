{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2022.Day19 where
import Util (getSample, getPuzzle, getNums)

samp :: IO String
puzz :: IO String
samp = getSample 19
puzz = getPuzzle 19

data State = State
  { _oBots    :: Int   -- ^ Ore Bots
  , _cBots    :: Int   -- ^ Clay Bots
  , _obsBots  :: Int   -- ^ Obsidian Bots
  , _gBots    :: Int   -- ^ Geode Bots
  , _ore      :: Int   -- ^ Ore
  , _clay     :: Int   -- ^ Clay
  , _obs      :: Int   -- ^ Obsidian
  , _geo      :: Int } -- ^ Geodes
  deriving (Show, Eq, Ord)

data Blueprint = Bp
  { _id       :: Int  
  , _oCost    :: Int    -- ^ ore bot cost in ore
  , _cCost    :: Int    -- ^ clay cost in ore
  , _obsCost1 :: Int    -- ^ obsidian cost in ore
  , _obsCost2 :: Int    -- ^ obsidian cost in clay
  , _gCost1   :: Int    -- ^ geo cost in ore
  , _gCost2   :: Int }  -- ^ geo cost in obsidian        
  deriving (Show, Eq, Ord)

parse :: String -> [Blueprint]
parse = 
  map parseLine . lines
  where
    parseLine :: String -> Blueprint
    parseLine s = 
      let [idx,oCost,cCost,obsCost1,obsCost2,gCost1,gCost2] = getNums s
      in Bp 
          { _id = idx
          , _oCost = oCost 
          , _cCost = cCost
          , _obsCost1 = obsCost1
          , _obsCost2 = obsCost2 
          , _gCost1 = gCost1
          , _gCost2 = gCost2 }