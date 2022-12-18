{-# OPTIONS_GHC -Wno-unused-imports #-}
module Y2022.Day16 where

import qualified Data.Set as S
import Data.Set        (Set)
import Data.List       (find, intercalate)
import Data.List.Split (splitOn)
import Util            (getNums, pairs, Parts (PartA), signOf, getSample, getPuzzle)
import Text.Regex.TDFA (AllTextMatches(..), (=~))
import Algorithm.Search (aStar)

samp :: IO String
samp = getSample 16
puzz :: IO String
puzz = getPuzzle 16

type ValveId = String
type Cost = Int

data Path = P 
  { _n1 :: ValveId
  , _n2 :: ValveId
  , _cost :: Cost }

instance Eq Path where
  (==) :: Path -> Path -> Bool
  P a1 b1 c1 == P a2 b2 c2 = 
    (c1 == c2) &&
       (a1 == a2 && b1 == b2) ||
       (a1 == b2 && b1 == a2) 

data Valve = Valve
  { _valve :: String
  , _flow :: Int
  , _leadsTo :: [String] }
  deriving (Eq)

instance Show Valve where
  show :: Valve -> String
  show Valve{_valve=v, _flow=f, _leadsTo=others} =
    "[Valve:" ++ v ++ "(" ++ show f ++ ") -> (" ++ intercalate "," others ++ ")]"

parse :: String -> [Valve]
parse = map parseLine . lines
  where
    parseLine :: String -> Valve
    parseLine s =
      let rate = getNums s
          valves = getAllTextMatches (s =~ ("[A-Z]{2}" :: String))
      in Valve {_valve = head valves, _flow = head rate, _leadsTo = tail valves}

toDot :: [Valve] -> String
toDot vs = 
  "digraph {\n" ++ header vs ++ "\n  " ++ edges vs ++ "\n}" 
  where
    toHeader :: Valve -> String
    toHeader Valve{_valve=v,_flow=f} = v ++ " [shape=record label=\""++ v ++ "|" ++ show f ++"\"]"
    header :: [Valve] -> String
    header = intercalate "\n  " .  map toHeader
    edge :: Valve -> String
    edge Valve {_valve=v, _leadsTo=others} = v ++ " -> {" ++ intercalate "," others ++ "}" 
    edges :: [Valve] -> String
    edges = intercalate "\n  " . map edge