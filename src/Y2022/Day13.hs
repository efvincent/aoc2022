{-# OPTIONS_GHC -Wno-unused-imports #-}
module Y2022.Day13 () where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List       (sort, foldl')
import Fifo            (Fifo, push, pop, fromList)
import Util            (getNums, Parts (..), getSample, getPuzzle)

samp :: IO String
samp = getSample 12
puzz :: IO String
puzz = getPuzzle 12


