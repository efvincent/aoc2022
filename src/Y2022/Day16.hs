{-# LANGUAGE BlockArguments #-}
module Y2022.Day16 
-- (sln16) 
where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Set        (Set)
import Data.Map        (Map)
import Data.List       (foldl', intercalate, tails)
import Text.Regex.TDFA (AllTextMatches(..), (=~))
import Data.Maybe      (maybeToList)
import Util            (getNums, Parts (..), getSample, getPuzzle)

samp = getSample 16
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

data Valve = V
  { _valve :: String
  , _flow :: Int
  , _leadsTo :: [String] }
  deriving (Eq)

instance Show Valve where
  show :: Valve -> String
  show V{_valve=v, _flow=f, _leadsTo=others} =
    "[Valve:" ++ v ++ "(" ++ show f ++ ") -> (" ++ intercalate "," others ++ ")]"

type Valves = Map String Valve

{-| Optimizes the input graph by eliminating zero benefit nodes and adjusting
    path costs to compensate. 
    
    Algorithm:
    
    1. find a node that has zero benefit (flow)
    2. find all paths in which it is involved
    3. find the set of all "other" nodes from the set of paths
    4. generate a new set of paths made of the "cross join" of nodes from (3), where
       the cost of each path is the sum of the cost of the two paths that came from (2)
       that include the 2 nodes in the new path
    -}
-- optimize :: Valves -> Valves
-- optimize valves =
--   let zfvs = M.filter (\(V v f _) -> v /= "AA" && f == 0)-- (z)ero(f)low(v)alve(s)
--   in opt zfvs
--   where
--     -- opt :: Graph -> [Valve] -> Graph
--     opt acc [] = acc
--     opt vs (v@V{_valve=name, _leadsTo=others}:t) =
--       let rawPaths  = nub [(a,b) | a <- others, b <- others, a < b]
--           newPaths  = S.fromList . map (\((n1,c1),(n2,c2)) -> P n1 n2 (c1+c2)) $ rawPaths
--           -- remove bad valve, bad paths, insert new paths
--           valves'   = remove v vs
--           paths'    = paths S.\\ (S.fromList . others) 

--           paths''   = paths' `S.union` newPaths
--       in  opt (acc{_valves=valves', _paths=paths''}) t


-- pathsFor :: String -> Graph -> [Path]
-- pathsFor s G{_paths=paths} =
--   S.toList . S.filter (\(P n1 n2 _) -> n1 == s || n2 == s) $ paths

parse :: String -> Valves
parse = M.fromList . map parseLine . lines
  where
    parseLine :: String -> (ValveId, Valve)
    parseLine s =
      let rate = getNums s
          valves = getAllTextMatches (s =~ ("[A-Z]{2}" :: String))
          vId = head valves
      in  (vId, V {_valve = vId, _flow = head rate, _leadsTo = tail valves})

toDot :: [Valve] -> String
toDot vs =
  "digraph {\n" ++ header vs ++ "\n  " ++ edges vs ++ "\n}"
  where
    toHeader V{_valve=v,_flow=f} = v ++ " [shape=record label=\""++ v ++ "|" ++ show f ++"\"]"
    header = intercalate "\n  " .  map toHeader
    edge V{_valve=v, _leadsTo=others} = v ++ " -> {" ++ intercalate "," others ++ "}"
    edges = intercalate "\n  " . map edge

floydWarshall :: Ord k => [k] -> Map (k,k) Int -> Map (k,k) Int
floydWarshall keys =
  each \k -> each \i -> each \j dists ->
    case (M.lookup (i,k) dists, M.lookup (k,j) dists) of
      (Just d1, Just d2) -> M.insertWith min (i,j) (d1 + d2) dists
      _cost              -> dists
  where
    each g z = foldl' (flip g) z keys

sln16 :: Parts -> String -> Int
sln16 part str =
  let valves = parse str
      valves' = M.elems valves
      dists1 = M.fromList [((vId,v),1) | V vId  _ vs <- valves', v <- vs]
      dists2 = floydWarshall [vId | V vId _ _ <- valves'] dists1
      flows  = M.fromList [(vId, n) | V vId n _ <- valves', n > 0]
      graph  = M.fromListWith (++)
                [(src, [(dst, cost+1, flow)])
                  | ((src,dst),cost) <- M.assocs dists2
                  , src == "AA" || M.member src flows, src /= dst
                  , flow <- maybeToList (M.lookup dst flows)]
  in
    if part == PartA then 
      let routeVals = solver graph 30 in maximum routeVals
    else
      let routeVals = solver graph 26
      in maximum [v1 + v2 | (open1, v1) : elephants <- tails (M.assocs routeVals),
                            (open2, v2)             <- elephants, S.disjoint open1 open2]
  where
    solver :: Map String [(String,Int,Int)] -> Int -> Map (Set String) Int
    solver graph time = M.fromListWith max (go [(time, "AA", S.empty, 0)])
      where
        go xs = [(open, flow) | (_,_,open,flow) <- xs] ++ concatMap (go . step) xs
        step (t, here, open, flow) =
          [(t', next, S.insert next open, flow + t' * valve)
            | (next, cost, valve) <- graph M.! here
            , not (S.member next open)
            , let t' = t - cost
            , t' > 0 ]