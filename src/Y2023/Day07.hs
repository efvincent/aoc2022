{-
failed part 2 ans: 
  250621362
  250625487
  250663015
  250557959
  250441902

  250577259
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sortOn" #-}
{-# LANGUAGE RankNTypes #-}
module Y2023.Day07 (parse, sln2307, solve, adjustHand, Card(..), s2, s1, s0, ss)  where

import Data.List (group, sortOn, sortBy)
import Data.Ord (comparing, Down (Down))
import Data.Foldable (minimumBy)
import Util (getPuzzle, getSample)

data Card =   Joker | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J | Q | K | A
  deriving (Show, Eq, Ord)

type Rank   = Int
type Bid    = Int
type Cards  = [Card]
type Hand   = (Cards, (Rank, Bid))
type Puzzle = [Hand]

sln2307 :: String -> (Int,Int)
sln2307 s =
  let puz = parse s in
  (solve id puz, solve (map adjustHand) puz)

solve :: (Puzzle -> Puzzle) -> Puzzle -> Int
solve adjFn puz =
  let puz' = zip [1..] (sortBy compHand . adjFn $ puz) in
  sum $ map (\(n, (_, (_, bid))) -> n * bid) puz'

compHand :: Hand -> Hand -> Ordering
compHand (hand1,(kind1,_)) (hand2,(kind2,_))
  | kind1 < kind2 = LT
  | kind1 > kind2 = GT
  | otherwise = compare hand1 hand2

parse :: String -> Puzzle
parse s =
  let ls    = lines s in
  let bids  = map (read . drop 5) ls in
  let cards = map (map parseCard . take 5) ls in
  let kinds = map rankOf cards in
  zip cards $ zip kinds bids

adjustHand :: Hand -> Hand
adjustHand hand@(cards,(_,bid))=
  let cards' = [if c == J then Joker else c | c <- cards] in
  case Joker `elem` cards' of
    False -> hand
    True | all (== Joker) cards' -> (cards', (6, bid))
    True ->
      let bestNonJoker = snd . minimumBy (comparing Down) . map (\g -> (length g, head g)) . group . sortOn id . filter (/= Joker) $ cards' in
      (cards',(rankOf wildcardSubHand, bid))

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

parseCard :: Char -> Card
parseCard c
  | c == '2' = N2 | c == '3' = N3 | c == '4' = N4 | c == '5' = N5
  | c == '6' = N6 | c == '7' = N7 | c == '8' = N8 | c == '9' = N9
  | c == 'T' = T  | c == 'J' = J  | c == 'Q' = Q  | c == 'K' = K
  | c == 'A' = A  | otherwise = undefined

ss :: IO ()
ss = do
  putStrLn "    \tGood?\tAns"
  s0
  s1
  s2

s0 :: IO ()
s0 = do
  raw <- getSample 7
  let puz = parse raw
  let ans = solve id puz
  putStrLn $ "samp 1\t" ++ show (ans == 6440) ++ "\t" ++ show ans
  let ans2 = solve (map adjustHand) puz
  putStrLn $ "samp 2\t" ++ show (ans2 == 5905) ++ "\t" ++ show ans2

s1 :: IO ()
s1 = do
  raw <- getPuzzle 7
  let puz = parse raw
  let ans = solve id puz
  let correct = 252295678
  let s = if ans == correct then "" else "\t(" ++ show (correct - ans) ++ ")"
  putStrLn $ " puz 1\t" ++ show (ans == 252295678) ++ "\t" ++ show ans ++ s

s2 :: IO ()
s2 = do
  raw <- getPuzzle 7
  let puz = parse raw
  let ans = solve (map adjustHand) puz
  let correct = 250577259
  let s = if ans == correct then "" else "\t(" ++ show (correct - ans) ++ ")"
  putStrLn $ " puz 2\t" ++ show (ans == 250577259) ++ "\t" ++ show ans ++ s
