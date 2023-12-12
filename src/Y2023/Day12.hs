{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2023.Day12 where

import Data.List (find)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Map.Strict as M (Map, toList, (!), insert, empty, member, (!))