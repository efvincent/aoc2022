{- DAY07 : https://adventofcode.com/2022/day/7 -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2022.Day07 (sln07A, sln07B) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Util (startsWith)

type DirEntries = M.Map String FSEntry

data FSEntry
  = File Int String
  | Dir Int String DirEntries
  deriving (Show, Eq, Ord)

data Command
  = CD String
  | UP
  | LS
  deriving (Show, Eq)

data AST
  = Cmd Command
  | FSE FSEntry
  deriving (Show, Eq)

parseLine :: String -> AST
parseLine [] = error "end of input"
parseLine s
  | s `startsWith` "$ cd .." = Cmd UP
  | s `startsWith` "$ cd "   = Cmd (CD (drop 5 s))
  | s `startsWith` "$ ls"    = Cmd LS
  | s `startsWith` "dir "    = FSE (Dir 0 (drop 4 s) M.empty)
  | otherwise                =
    let [rawSize, filename] = splitOn " " s in
    FSE (File (read rawSize) filename)

parseInput :: String -> [AST]
parseInput = map parseLine . lines

addEntries :: DirEntries -> [AST] -> (DirEntries, [AST])
addEntries de [] = (de,[])
addEntries de ast@(h:t) =
  case h of
    Cmd _ -> (de, ast)
    FSE subDir@(Dir _ name _) ->
      case de M.!? name of
        Just _ -> addEntries de t
        Nothing -> addEntries (M.insert name subDir de) t
    FSE file@(File _ name) ->
      addEntries (M.insert name file de) t

interpret :: [AST] -> FSEntry
interpret = go []
  where
    go :: [FSEntry] -> [AST] -> FSEntry
    go [] [] = error "invariant violation, no root, no commands"
    go [_] ((Cmd UP):_) = error "invariant violation, up command from root"

    -- no more commands, at root, return root
    go [root] [] = root

    -- command to cd when there's no stack
    go [] ((Cmd (CD n)):t) = go [Dir 0 n M.empty] t

    -- tracking back up the stack, no more commands
    go stack@((Dir _ n _):ancestors) []
      | n /= "/" = go stack [Cmd UP]
      | otherwise = go ancestors []

    -- command to go up one directory
    go ((Dir _ n entries):(Dir _ pn pe):ancestors) ((Cmd UP):t) =
      go (Dir 0 pn (M.insert n (Dir 0 n entries) pe):ancestors) t

    -- command to list the contents of the current directory. From the
    -- commands process directory entries (FSE) until you get to the end or
    -- a command Cmd
    go ((Dir wdSz n entries):ancestors) ((Cmd LS):t) =
      let (entries', cmds) = addEntries entries t in
      go (Dir wdSz n entries':ancestors) cmds

    -- command to change into a subdirectory
    go ((Dir dSz dName entries):ancestors) ((Cmd (CD subDirName)):t) =
      let subdir =
            case entries M.!? subDirName of
              Just subDir -> subDir
              Nothing -> Dir 0 subDirName M.empty
          entries' = M.insert subDirName subdir entries
          newCurDir = Dir dSz dName entries'
      in go (subdir:newCurDir:ancestors) t

    go dirs cmds =
      error $ "invariant violation - unhandled interpret:\ndirs: "
        ++ show dirs ++ "\ncmds: " ++ show cmds

updateSize :: FSEntry -> FSEntry
updateSize = fst . go
  where
    go :: FSEntry -> (FSEntry,Int)
    go f@(File s _) = (f, s)
    go (Dir _ name contents) =
      let
        entries   = map go . M.elems $ contents
        contents' = M.fromList . map toEntry $ entries
        sz        = sum . map snd $ entries
      in (Dir sz name contents', sz)
    toEntry (e@(Dir _ n _),_) = (n,e)
    toEntry (e@(File _ n) ,_) = (n,e)

dirSizes :: (Int -> Bool) -> FSEntry -> [Int]
dirSizes predicate root =
  go [] [root]
  where
    go acc [] = acc
    go acc ((File _ _):t) = go acc t
    go acc ((Dir sz _ entries):t)
      | predicate sz = go (sz:acc) (map snd (M.toList entries) ++ t)
      | otherwise    = go acc (map snd (M.toList entries) ++ t)

sln07A :: String -> String
sln07A =
  show . sum . dirSizes (<= 100000) . updateSize . interpret . parseInput

sln07B :: String -> String
sln07B s =
  let
    totalSpace        = 70000000
    targetUnused      = 30000000
    root@(Dir sz _ _) = updateSize . interpret . parseInput $ s
    available         = totalSpace - sz
    reqd              = targetUnused - available
    sizes             = dirSizes (const True) root in
  show . minimum . filter (>= reqd) $ sizes