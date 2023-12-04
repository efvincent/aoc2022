{-# LANGUAGE RecordWildCards #-}
module Y2023.Day03
  -- (parse)  where
  where

import Coord (cY, origin, right, Coord(..), allNeighbors )
import Util (getSample, getPuzzle)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isNumber, digitToInt)
import Data.Maybe (fromMaybe)
import Data.List (sort)

type PartNum = ((Coord, Coord), Int)
type Sym = (Coord, Char)
type SymMap = M.Map Int (M.Map Int [Sym])
type PartMap = M.Map Int (M.Map Int [PartNum])

data Puzzle = Puzzle
  { parts    :: [PartNum]
  , symbols  :: [Sym]
  , symMapY  :: SymMap
  , partMapY :: PartMap }
  deriving (Show)

ssz :: Coord
ssz = C 9 9
psz :: Coord
psz = C 110 110

ppuz :: Coord -> Puzzle -> IO ()
ppuz (C my mx) Puzzle{..} = do
  putStrLn "    0 1 2 3 4 5 6 7 8 9"
  putStrLn "  /--------------------"
  putStr "0 |"
  loop (C 0 0) allPoints
  where
    mkNumPoints :: PartNum -> [(Coord, Char)]
    mkNumPoints ((C sy sx, _), num) =
      let digits = show num in
      let dl = length digits in
      (zipWith (\i c -> (C sy (sx + i), c)) [0..dl-1] . show) num

    allPoints = [C y x | y <- [0..my], x <- [0..mx]]
    allParts = parts >>= mkNumPoints
    symMap =  M.fromList (symbols ++ allParts)
    -- numMap = M.fromList $ map (\)
    loop _ [] = do putStrLn "\ndone"
    loop (C curY curX) points@(curPos@(C y _):rest)
      | curY == y =
        case M.lookup curPos symMap of
          Nothing -> do
            putStr " ·"
            loop (C curY (curX + 1)) rest
          Just ch -> do
            putStr (' ':[ch])
            loop (C curY (curX + 1)) rest
      | otherwise = do
        let curY' = curY + 1
        putStrLn ""
        putStr $ show curY' ++ " |"
        loop (C curY' curX) points

sln2302 :: String -> (Int, Int)
sln2302 s = (solve1 s, solve2 s)

pz0 :: Puzzle
pz0 = Puzzle
  { parts = []
  , symbols = []
  , symMapY = M.singleton 0 M.empty
  , partMapY = M.singleton 0 M.empty }

isSymbol :: Char -> Bool
isSymbol c = c /= '.' && not (isNumber c)

readNum :: (Int,Int) -> String -> (Int,Int)
readNum acc [] = acc
readNum acc@(n,len) (c:rest)
  | isNumber c = readNum (n*10 + digitToInt c, len+1) rest
  | otherwise = acc

addSym :: Sym -> Puzzle -> Puzzle
addSym sym@(C y x, _) p@Puzzle{..} =
  let symMapY' =
        let xm = M.findWithDefault M.empty y symMapY in
        let syms = M.findWithDefault [] x xm in
        let xm' = M.insert x (sym:syms) xm in
        M.insert y xm' symMapY in
  p { symbols = sym:symbols, symMapY = symMapY' }

addPartCoordToMap :: PartMap -> Coord -> PartNum -> PartMap
addPartCoordToMap m (C y x) part =
  let xm = M.findWithDefault M.empty y m in
  let ns = M.findWithDefault [] x xm in
  let xm' = M.insert x (part:ns) xm in
  M.insert y xm' m

addPart :: String -> Coord -> Puzzle -> (Coord,Puzzle)
addPart str pos@(C y x) puz@Puzzle{..} =
  let (n,l) = readNum (0,0) str in
  let numIdxs = [C y x' | x' <- [x .. (x + l - 1)]] in
  let part = ((pos, C y (x + l - 1)), n) in
  let partMapY' = foldl (\acc p -> addPartCoordToMap acc p part) partMapY numIdxs in
  let puz' = puz { parts = part:parts, partMapY = partMapY' } in
  scanLine (pos + C 0 l) puz' (drop l str)

scanLine :: Coord -> Puzzle -> String -> (Coord, Puzzle)
scanLine pos puz [] =
  let pos' = C (cY pos + 1) 0 in
  (pos', puz)
scanLine pos puz str@(c:rest)
  | c == '.' = scanLine (right pos) puz rest
  | isSymbol c =
    let acc' = addSym (pos, c) puz in
    scanLine (right pos) acc' rest
  | otherwise =
    addPart str pos puz

symNextToPart :: Puzzle -> Coord -> Bool
symNextToPart Puzzle{..}  partCoord@(C y x) =
  case symMapY M.!? y of
    Nothing -> False
    Just xMap ->
      case xMap M.!? x of
        Nothing -> False
        Just syms ->
          let symPoss = map fst syms in
          elem partCoord symPoss

pointInRange :: Coord -> (Coord,Coord) -> Bool
pointInRange  (C y x) (C y1 x1, C y2 x2)=
  let pts = [C y' x' | y' <- [y1..y2], x' <- [x1..x2]] in
  any (\(C py px) -> py == y && px == x) pts

numsNextToSym :: Puzzle -> Coord -> [Int]
numsNextToSym Puzzle{..} sym = 
  let nums = map snd . S.toList . S.fromList $ allNeighbors sym >>= loop in 
  if length nums == 2 then nums else []
  where 
  loop pos@(C y x) =
    let candidates =
          case partMapY M.!? y of
            Nothing -> []
            Just xMap -> S.toList . S.fromList . fromMaybe [] $ (xMap M.!? x) in
    filter (pointInRange pos . fst) candidates

isPartNum :: Puzzle -> PartNum -> Bool
isPartNum puz part  =
  let ((C y x1, C _ x2), _) = part in
  let partCoords = map (C y) [x1..x2] in
  let partCoords' = S.union (S.fromList partCoords) (S.fromList $ partCoords >>= allNeighbors) in
  any (symNextToPart puz) partCoords'

solve1 :: String -> Int
solve1 s =
  let puz@Puzzle{..} = parse s in
  sum . map snd . filter (isPartNum puz) $ parts

solve2 :: String -> Int
solve2 s =
  let puz@Puzzle{..} = parse s in
  let symbols' = map fst . filter ((== '*') . snd) $ symbols in
  sum . map product . filter ((== 2) . length) . map (numsNextToSym puz) $ symbols'

parse :: String -> Puzzle
parse = snd . foldl (\(pos,acc) l -> scanLine pos acc l) (origin,pz0) . lines