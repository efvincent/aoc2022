{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sortOn" #-}
module Y2023.Day07 (sln2307)  where

import Data.List (group, sortOn, sortBy)
import Data.Ord (comparing, Down (Down))
import Data.Foldable (minimumBy)
import Data.Char (digitToInt)
import GHC.Unicode (isDigit)
import Util (replaceAll)

data Card =   Joker | N Int | T | J | Q | K | A
  deriving (Show, Eq, Ord)

type Rank   = Int
type Bid    = Int
type Cards  = [Card]
type Hand   = (Cards, (Rank, Bid))
type Puzzle = [Hand]

{-- Solutions----------------------------------------------------}

sln2307 :: String -> (Int,Int)
sln2307 s =
  let puz = parse s in
  (solve id puz, solve (map jokerize) puz)

solve :: (Puzzle -> Puzzle) -> Puzzle -> Int
solve adjuster puz =
  let puz' = zip [1..] (sortBy compareHands . adjuster $ puz) in
  sum $ map (\(n, (_, (_, bid))) -> n * bid) puz'

jokerize :: Hand -> Hand
jokerize hand@(cards,(_,bid))=
  let cards' = [if c == J then Joker else c | c <- cards] in
  case Joker `elem` cards' of
    False -> hand
    True | all (== Joker) cards' -> (cards', (6, bid))
    True ->
      let bestNonJoker = snd . minimumBy (comparing Down) . map (\g -> (length g, head g)) . group . sortOn id . filter (/= Joker) $ cards' in
      let wildcardSubHand = replaceAll Joker bestNonJoker cards' in 
      (cards',(rankOf wildcardSubHand, bid))

compareHands :: Hand -> Hand -> Ordering
compareHands (hand1,(kind1,_)) (hand2,(kind2,_))
  | kind1 < kind2 = LT
  | kind1 > kind2 = GT
  | otherwise = compare hand1 hand2

rankOf :: Cards -> Int
rankOf h =
  let grps = sortBy (comparing Down) . map length . group . sortOn id $ h in
  case grps of
    [5]       -> 6
    [4,1]     -> 5
    [3,2]     -> 4
    [3,1,1]   -> 3
    [2,2,1]   -> 2
    [2,1,1,1] -> 1
    _         -> 0

{-- Parsing ----------------------------------------------------}

parse :: String -> Puzzle
parse s =
  let ls    = lines s in
  let bids  = map (read . drop 5) ls in
  let cards = map (map parseCard . take 5) ls in
  let kinds = map rankOf cards in
  zip cards $ zip kinds bids

parseCard :: Char -> Card
parseCard c
  | isDigit c = N (digitToInt c)
  | c == 'T' = T  | c == 'J' = J  | c == 'Q' = Q  | c == 'K' = K
  | c == 'A' = A  | otherwise = undefined

