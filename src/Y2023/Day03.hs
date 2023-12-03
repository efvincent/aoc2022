{-# LANGUAGE RecordWildCards #-}
module Y2023.Day03  where

import Coord
import qualified Data.Map as M
import Data.Char (isNumber, digitToInt)

type PartNum = ((Coord, Coord), Int)
type Sym = (Coord, Char)
data Puzzle = Puzzle
  { parts::[PartNum]
  , symbols::[Sym] }
  deriving (Show)

pz0 :: Puzzle
pz0 = Puzzle { parts = [], symbols = [] }

isSymbol :: Char -> Bool
isSymbol c = c /= '.' && not (isNumber c)

readNum :: (Int,Int) -> String -> (Int,Int)
readNum acc [] = acc
readNum acc@(n,len) (c:rest)
  | isNumber c = readNum (n*10 + digitToInt c, len+1) rest
  | otherwise = acc

scanLine :: Coord -> Puzzle -> String -> (Coord, Puzzle)
scanLine pos acc [] = 
  let pos' = C (cY pos + 1) 0 in
  (pos', acc)
scanLine pos@(C y x) acc@Puzzle{..} str@(c:rest)
  | c == '.' = scanLine (right pos) acc rest
  | isSymbol c =
    let sym = (pos, c) in
    let acc' = acc { symbols = sym:symbols } in
    scanLine (right pos) acc' rest
  | otherwise =
    let (n,l) = readNum (0,0) str in
    let part = ((pos, C y (x + l - 1)), n) in
    let acc' = acc { parts = part:parts } in
    scanLine (pos + C 0 (x + l)) acc' (drop (l - 1) rest)

-- parse :: String -> Puzzle
parse s =
  let ls = lines s in
  let orig = C 0 0 in
  let result@(endPos, puz) = foldl (\(pos,acc) l -> scanLine pos acc l) (orig,pz0) ls in
  result