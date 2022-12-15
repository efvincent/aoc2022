{-# OPTIONS_GHC -Wno-unused-imports #-}
module Y2022.Day16 where

import qualified Data.Set as S
import Data.Set        (Set)
import Data.List       (find)
import Data.List.Split (splitOn)
import Util            (getNums, pairs, Parts (PartA), signOf, getSample, getPuzzle)

samp :: IO String
samp = getSample 16
puzz :: IO String
puzz = getPuzzle 16

