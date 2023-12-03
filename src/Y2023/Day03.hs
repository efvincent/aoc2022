{-# LANGUAGE RecordWildCards #-}
module Y2023.Day03 
  -- (parse, adjacentToSym)  where
  where

import Coord
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isNumber, digitToInt)
import Data.List (intersect)
import Debug.Trace

type PartNum = ((Coord, Coord), Int)
type Sym = (Coord, Char)
data Puzzle = Puzzle
  { parts::[PartNum]
  , yMap :: M.Map Int (M.Map Int [Sym]) }   -- map row (map col sym)
  deriving (Show)

pz0 :: Puzzle
pz0 = Puzzle { parts = [], yMap = M.singleton 0 M.empty }

isSymbol :: Char -> Bool
isSymbol c = c /= '.' && not (isNumber c)

readNum :: (Int,Int) -> String -> (Int,Int)
readNum acc [] = acc
readNum acc@(n,len) (c:rest)
  | isNumber c = readNum (n*10 + digitToInt c, len+1) rest
  | otherwise = acc

addSym :: Sym -> Puzzle -> Puzzle
addSym sym@(C y x, c) p@Puzzle{..} =
  let (yIdx, xIdx) = trace ("sym " ++ show c ++ " at " ++ show (C y x) ++ " has indexes " ++ show (y `div` 3, x `div` 3)) (y `div` 3, x `div` 3) in
  let yMap' =
        let xm = M.findWithDefault M.empty yIdx yMap in
        let symbols = M.findWithDefault [] xIdx xm in
        let xm' = M.insert xIdx (sym:symbols) xm in
        M.insert yIdx xm' yMap in
  p { yMap = yMap' }

scanLine :: Coord -> Puzzle -> String -> (Coord, Puzzle)
scanLine pos acc [] =
  let pos' = C (cY pos + 1) 0 in
  (pos', acc)
scanLine pos@(C y x) puz@Puzzle{..} str@(c:rest)
  | c == '.' = scanLine (right pos) puz rest
  | isSymbol c =
    let acc' = addSym (pos, c) puz in
    let acc'' = trace ("symbol " ++ show c ++ " at " ++ show pos) acc' in
    scanLine (right pos) acc'' rest
  | otherwise =
    let (n,l) = readNum (0,0) str in
    let part = ((pos, C y (x + l - 1)), n) in
    let acc' = puz { parts = part:parts } in
    scanLine (pos + C 0 l) acc' (drop l str)

adjacentToSym :: Puzzle -> Coord -> Bool
adjacentToSym Puzzle{..}  partCoord@(C y x) =
  let (yIdx', xIdx') = (y `div` 3, x `div` 3) in
  let (yIdx,xIdx) = trace ("digit at " ++ show partCoord ++ " has indexes " ++ show (yIdx',xIdx'))  (yIdx',xIdx') in
  case yMap M.!? yIdx of
    Nothing -> False
    Just xMap ->
      case xMap M.!? xIdx of
        Nothing -> False
        Just syms' ->
          let syms = trace ("syms: " ++ show syms') syms' in
          let symPoss = map fst syms in
          let nbors' = allNeighbors partCoord in
          let (_,nbors) = trace ("neighbors: " ++ show nbors') (syms, nbors') in
          let inter = trace ("intersection: " ++ show (nbors `intersect` symPoss)) intersect nbors symPoss in
          not $ null inter

isPartNum :: Puzzle -> PartNum -> Bool
isPartNum puz part'  =
  let part = trace ("\npart: " ++ show part') part' in
  let ((C y x1, C _ x2), _) = part in
  let partCoords = map (C y) [x1..x2] in
  let partCoords' = S.union (S.fromList partCoords) (S.fromList $ partCoords >>= allNeighbors) in
  any (adjacentToSym puz) partCoords'

-- parse :: String -> Puzzle
parse s =
  let ls = lines s in
  let result@(endPos, puz@Puzzle{..}) = foldl (\(pos,acc) l -> scanLine pos acc l) (origin,pz0) ls in
  let parts' = filter (isPartNum puz) $ parts in
  (puz,parts')