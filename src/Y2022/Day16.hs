{-# LANGUAGE BlockArguments #-}
module Y2022.Day16
-- (sln16) 
where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Set        (Set)
import Data.Map        (Map)
import Data.List       (nub, foldl', intercalate, tails)
import Text.Regex.TDFA (AllTextMatches(..), (=~))
import Data.Maybe ( maybeToList, mapMaybe )
import Util            (getNums, Parts (..), getSample, getPuzzle)

samp = getSample 16
puzz = getPuzzle 16

type ValveId = String
type Cost = Int

data Valve = V
  { _valveId :: ValveId
  , _flow :: Int
  , _leadsTo :: [(ValveId, Cost)] }
  deriving (Eq)

instance Show Valve where
  show :: Valve -> String
  show V{_valveId=v, _flow=f, _leadsTo=others} =
    "[Valve:" ++ v ++ "(" ++ show f ++ ") -> (" ++ intercalate "," (map show others) ++ ")]"

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
optimize :: Valves -> Valves
optimize valves =
  let zfvs = filter (\(V v f _) -> v /= "AA" && f == 0) . M.elems $ valves -- (z)ero(f)low(v)alve(s)
      -- paths = concatMap (\(_,V n _ xs) -> map (n,) xs) . M.toList $ valves
  in opt valves zfvs
  where
    opt ::
      Valves {-^ all valves -} ->
      [Valve] {-^ zero flow valves -} ->
      Valves {-^ valves with zeros removed, and paths, costs adjusted -}
    opt acc [] = acc
    {-  each time through, take the next zero valve
        calculate paths we need if we eliminate this node, and the cost of that combined path
         -}
    opt acc (V{_valveId=name, _leadsTo=others}:t) =
          -- | new paths to create
      let paths0  = nub [(a,b) | a <- others, b <- others, a < b]

          -- | new paths including the cost
          newPaths  = map (\((n1,c1),(n2,c2)) -> (n1, n2, c1+c2)) paths0

          -- | zero flow valve removed from valves          
          valves'   = M.delete name acc

          -- | emove the current zero flow valve from any existing paths, and
          --   add the @newPaths@ with their costs to the remaining nodes
          valves'' = M.mapMaybe (\toModify -> rePath toModify name newPaths) valves'
          -- paths'    = paths S.\\ (S.fromList . others)
          -- paths''   = paths' `S.union` newPaths
      in  opt valves'' t --0  opt (acc{_valves=valves', _paths=paths''}) t
      where
        {-| return nothing if the valve is the one to be eliminated, otherwise, 
            if the valve needs to have a new destination valve & cost added do so,
            and if the valve to delete is in it's route list, remove it -}
        rePath ::
          Valve                      {-| the valve being modified -} ->
          ValveId                    {-| the ID of the valve to delete -} ->
          [(ValveId, ValveId, Cost)] {-| the list of new paths to create -} ->
          Maybe Valve
        rePath (V vId f o) toDelete newPaths
          | vId == toDelete = Nothing
          | otherwise =
            let others' = filter (\(curId, _) -> curId /= toDelete) o
                others'' = others' ++ mapMaybe (\(v1,v2,c) -> if v1 == vId then Just (v2,c)
                                                              else if v2 == vId then Just (v1,c)
                                                              else Nothing) newPaths
            in Just (V vId f others'')
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
      in  (vId, V {_valveId = vId, _flow = head rate, _leadsTo = map (,1) . tail $ valves})

toDot :: [Valve] -> String
toDot vs =
  "graph {\n" ++ header vs ++ "\n  " ++ edgesForNode vs ++ "\n}"
  where
    edges :: ValveId -> (ValveId, Cost) -> String
    edges v1 (v2, c) = v1 ++ " -- " ++ v2 ++ " [label=\" " ++ show c ++ "\"]" 
    toHeader V{_valveId=v,_flow=f} = v ++ " [shape=record label=\""++ v ++ "|" ++ show f ++"\"]"
    header = intercalate "\n  " .  map toHeader
    edge V{_valveId=v, _leadsTo=others} = 
      let others' = filter (\(o,_) -> o < v) others
      in intercalate "\n  " $ map (edges v) others'  
    edgesForNode = intercalate "\n  " . map edge 

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
  let valves = parse $ str
      valves' = M.elems valves
      dists1 = M.fromList [((vId,v),c) | V vId  _ vs <- valves', (v,c) <- vs]
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