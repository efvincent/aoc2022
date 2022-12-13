{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2022.Day11 (sln11) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List       (sort, foldl')
import Fifo            (Fifo, push, pop, fromList)
import Util            (getNums, Parts (..))

{-- Types & Consts-------------------------------------------------}

data Monkey = Monkey
  { _items :: Fifo Int
  , _insp  :: Int
  , _op    :: Op
  , _divBy :: Int
  , _true  :: Int
  , _false :: Int }
  deriving Show

type Monkeys = M.Map Int Monkey

{-| @Operand@ and @Op@ work together to build a super simple AST for
    the functions that each monkey has as their "operation"; allowing
    us to parse the input data into an AST that can be interpreted.
    See @Op@ for more detail -}
data Operand = Old | N Int
instance Show Operand where
  show :: Operand -> String
  show Old = "old"
  show (N n) = show n

{-| An AST for the "operations" each monkey in the puzzle has defined.
    Each operation is either an addition or multiplication, and the
    operands are each either "old" indicating the previous value, or
    a constant Int. 
    
    The @Show@ instances for Op and Operand are helpful when working
    in the repl to be able to see values. The 3rd item in the 4-tuple
    is a function, and functions don't have a Show instance and one
    cannot be derived, hence the custom Show instance -}
newtype Op = Op (Operand, String, Int -> Int -> Int, Operand)
instance Show Op where
  show :: Op -> String
  show (Op (op1, fnName, _, op2)) = show op1 ++ " " ++ fnName ++ " " ++ show op2

{-- Solutions----------------------------------------------------}

{-| Given the abstract representation of a Monkey function, 
    executed it on the given `old` value -}
runOp :: Int -> Op -> Int
runOp old (Op (op1, _ ,fn ,op2)) =
  let n1 = opVal old op1
      n2 = opVal old op2
  in n1 `fn` n2
  where
    opVal _ (N n) = n
    opVal n _     = n

{-| steps a monkey through one turn. "inspect" means apply the op
    in each step the monkey and another monkey may change -}
step :: Parts -> Int -> Monkeys -> Int -> Monkeys
step part superMod ms n =
  let m = ms M.! n in
      case pop . _items $ m of
        (items, Just item) ->
          let mkSmall = if part == PartA then flip div 3 else flip mod superMod
              item'   = mkSmall . runOp item . _op $ m
              throwTo = if item' `mod` _divBy m == 0 then _true m else _false m
              other   = ms M.! throwTo
              otherQ  = push item' $ _items other
              other'  = other { _items = otherQ }
              m'      = m { _items = items, _insp = _insp m + 1 }
              ms''    = M.insert n m' (M.insert throwTo other' ms)
          in step part superMod ms'' n
        _ -> ms

{-| steps all the monkeys through one round -}
runRound :: Parts -> Int -> Monkeys -> Monkeys
runRound part superMod ms = foldl' (step part superMod) ms [0..length ms - 1]

{-| the solution -}
sln11 :: Parts -> String -> Int
sln11 part raw =
  let iters = if part == PartA then 20 else 10000 :: Int
      monkeys = parse raw
      superMod = foldl' (*) 1 . M.elems . M.map _divBy $ monkeys in
  foldl' (*) 1 . take 2 . reverse . sort . M.elems . M.map _insp . 
  foldl' (\acc _ -> runRound part superMod acc) monkeys $ [1..iters]

{-- Helpers -----------------------------------------------------}

parse :: String -> Monkeys
parse =
  M.fromList . zipWith (curry parseMonkey) [0..] . splitOn "\n\n"

parseMonkey :: (Int,String) -> (Int,Monkey)
parseMonkey (n, raw) =
  let [rItems, rOp, rTest, rT, rF] = drop 1 . lines $ raw
      m = Monkey
        { _items = pItems rItems
        , _insp  = 0
        , _op    = pOp rOp
        , _divBy = pTest rTest
        , _true  = head . getNums $ rT
        , _false = head . getNums $ rF }
  in (n, m)
  where
    pItems :: String -> Fifo Int
    pItems = fromList . map toEnum . getNums

    pTest :: String -> Int
    pTest = head . map toEnum . getNums

    pArg :: String -> Operand
    pArg "old" = Old
    pArg s = N (read s)

    pOp :: String -> Op
    pOp rOp =
      let [a1, o, a2] = words . (!! 1) . splitOn "=" $ rOp
          (op,opName) = if o == "*" then ((*),"*") else ((+),"+")
      in  Op (pArg a1, opName, op, pArg a2)

