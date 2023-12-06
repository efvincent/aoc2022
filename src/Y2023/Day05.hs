{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2023.Day05 (sln2305) where

import Data.List.Split (splitOn, chunksOf)
import Util (getNums, Parts(..)) 
import Data.Ix (inRange, index)

type Range      = (Int,Int)     -- start, end inclusive
type MapLine    = (Range,Range) -- source range, dest range
type CatalogMap = [MapLine]
data Seeds      = Singles [Int] | Ranges [Range]
type Puzzle     = (Seeds, [CatalogMap])

{-- Solutions----------------------------------------------------}

sln2305 :: String -> (Int, Int)
sln2305 s =
  let p1 = let (Singles ss, ms) = parse s PartA in 
          minimum . map (\n -> foldl decodeMap n ms) $ ss in
  let p2 = let (Ranges ss, ms) = parse s PartB in 
          fst . minimum . foldl (decodeLevel []) ss $ ms in
  (p1,p2)

{-- Solution Part 1 ---------------------------------------------}

decodeMap :: Int -> CatalogMap -> Int
decodeMap n []= n
decodeMap n (l:rest) =
  case decodeLine n l of
    Just n' -> n'
    Nothing -> decodeMap n rest

decodeLine :: Int -> MapLine -> Maybe Int
decodeLine n (src@(ss,_), (ds,_)) =
  if inRange src n
  then Just $ ds + n - ss
  else Nothing

{-- Solution Part 2 -------------------------------------------}

decodeLevel :: [Range] -> [Range] -> CatalogMap -> [Range] 
decodeLevel acc [] _ = acc
decodeLevel acc (input:rest) catMap =
  let acc' = acc ++ decodeMapOfRange [] input catMap in
  decodeLevel acc' rest catMap

decodeMapOfRange :: [Range] -> Range -> CatalogMap -> [Range]
decodeMapOfRange acc input [] = input : acc
decodeMapOfRange acc input (m:rest) =
  let (mapped, overflows) = decodeLineOfRange input m in
  let acc' = acc ++ mapped in
  if null overflows
  then acc'
  else overflows >>= (\overflow -> decodeMapOfRange acc' overflow rest)

{-| takes an input range and map line, returns a list of successfully mapped
    ranges and a list of unmapped ranges -}
decodeLineOfRange :: Range -> MapLine -> ([Range], [Range])
decodeLineOfRange input@(start,end) (src@(ss,se), target@(ds,de)) 
  | start < ss && end > se =
    -- range completely encompasses source, there will be two overflow ranges
    ([target], [(start, ss-1), (se+1, end)])
  | inRange src start && inRange src end =
    -- entire range is mapped
    ([(ds + start - ss, ds + end - ss)], [])
  | inRange src start = 
    -- start of range in map, overflows
    ([(ds + start - ss, de)], [(se + 1, end)])
  | inRange src end =
    -- end is in range, start is not
    let i = index src end in
    ([(ds, ds + i)], [(start, end - i - 1)])
  | otherwise = 
    -- entire range is unprocessed
    ([], [input])

{-- Parsing ----------------------------------------------------}

parse :: String -> Parts -> Puzzle
parse s p = case p of
  PartA ->
    let (rawSeeds:rawMaps) = splitOn "\n\n" s in
    let seeds = getNums rawSeeds in
    let maps = map (map parseMapLine . drop 1 . lines) rawMaps in
    (Singles seeds, maps)
  PartB ->
    let (rawSeeds:rawMaps) = splitOn "\n\n" s in
    let seeds = map (\[start,size] -> (start,start+size)) . chunksOf 2 . getNums $ rawSeeds in
    let maps = map (map parseMapLine . drop 1 . lines) rawMaps in
    (Ranges seeds, maps)

parseMapLine :: String -> (Range, Range)
parseMapLine s =
  let [ds,ss,l] = getNums s in
  ((ss, ss + l - 1), (ds, ds + l - 1))