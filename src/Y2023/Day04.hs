{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2023.Day04 (sln2304) where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M
import Util (getNums)   -- gets all the natural #s from a string using regex

type Card = (Int, Int)
type Puz = [Card]

sln2304 :: String -> (Int,Int)
sln2304 s = let puz = parse s in (solve1 puz, solve2 puz)

solve1 :: Puz -> Int
solve1 =
  let score n = if n == 0 then 0 else 2 ^ (n - 1) in
  sum . map (score . snd)

solve2 :: Puz -> Int
solve2 puz =
  loop 1 . M.fromList . map (\(gn, matches) -> (gn, (1, matches))) $ puz
  where
    mx = length puz
    loop :: Int -> M.Map Int (Int,Int) -> Int
    loop cardNum gameMap
      | cardNum > mx = M.foldl (\total (count,_) -> total + count) 0 gameMap
      | otherwise =
        let (curCount, matches) = gameMap M.! cardNum in
        let cardsToUpdate = map (+ cardNum) [1..matches] in
        let gameMap' = foldl (flip (M.adjust (\(c,w) -> (c + curCount, w)))) 
              gameMap cardsToUpdate in
        loop (cardNum + 1) gameMap'

parse :: String -> Puz
parse = map parseLine . lines

parseLine :: String -> Card
parseLine s =
  let [part1,playedNumsS] = splitOn "|" s in
  let [cardNumS,winnersS] = splitOn ":" part1 in
  let winners = S.fromList . getNums $ winnersS in
  let cards   = S.fromList . getNums $ playedNumsS in
  let cardNum = head . getNums $ cardNumS in
  (cardNum, length $ S.intersection winners cards)
