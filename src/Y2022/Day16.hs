{-# LANGUAGE BlockArguments #-}
module Y2022.Day16 (sln16) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Set        (Set)
import Data.Map        (Map)
import Data.List       (foldl', intercalate, tails)
import Text.Regex.TDFA (AllTextMatches (..), (=~))
import Data.Maybe      ( maybeToList)
import Util            (getNums, Parts (..))

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

parse :: String -> Valves
parse = M.fromList . map parseLine . lines
  where
    parseLine :: String -> (ValveId, Valve)
    parseLine s =
      let rate = getNums s
          valves = getAllTextMatches (s =~ ("[A-Z]{2}" :: String))
          vId = head valves
      in  (vId, V {_valveId = vId, _flow = head rate, _leadsTo = map (,1) . tail $ valves})

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