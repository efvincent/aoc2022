{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2022.Day07 where

import Util (Parts(..), getNums, getSample, getPuzzle, solve)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Debug.Trace (trace)

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

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith target search
  | length search > length target || null search || null target = False
  | otherwise =
    let target' = take (length search) target in
    search == target'

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
      let t' = trace (name ++ " (dir)") t in
      case de M.!? name of
        Just _ -> addEntries de t'
        Nothing -> addEntries (M.insert name subDir de) t'
    FSE file@(File sz name) ->
      let name' = trace (name ++ " " ++ show sz ++ " (file)") name in
      addEntries (M.insert name' file de) t

interpret :: [AST] -> FSEntry
interpret = go []
  where
    go :: [FSEntry] -> [AST] -> FSEntry
    go [] [] = error "invariant violation, no root, no commands"
    go [_] ((Cmd UP):_) = error "invariant violation, up command from root"

    -- no more commands, at root, return root
    go [root] [] = root

    -- command to cd when there's no stack
    go [] ((Cmd (CD subDirName)):t) =
      let t' = trace ("CD " ++ subDirName ++ " from root") t in
      go [Dir 0 subDirName M.empty] t'

    -- tracking back up the stack, no more commands
    go stack@((Dir _ n _):ancestors) []
      | n /= "/" = go stack [Cmd UP]
      | otherwise = go ancestors []

    -- command to go up one directory
    go ((Dir _ name contents):(Dir _ parentName parentContents):ancestors) ((Cmd UP):t) =
      let t' = trace ("Cmd UP") t in
      go ((Dir 0 parentName (M.insert name (Dir 0 name contents) parentContents)):ancestors) t'

    -- command to list the contents of the current directory. From the
    -- commands process directory entries (FSE) until you get to the end or
    -- a command Cmd
    go ((Dir wdSz name contents):ancestors) ((Cmd LS):t) =
      let
          t' = trace ("ls from " ++ name) t
          (contents', cmds) = addEntries contents t' in
      go (Dir wdSz name (trace (" \\-> new contents of " ++ name ++ ": " ++ show contents') contents'):ancestors) cmds

    -- command to change into a subdirectory
    go ((Dir wdSz wsName contents):ancestors) ((Cmd (CD subDirName)):t) =
      -- create or access the child
      -- add the child to the current directory
      -- replace working directory in stack
      -- push subdir to the top of the stack & continue 
      let
          subdirName' = trace ("CD " ++ subDirName ++ " from " ++ wsName) subDirName
          subdir =
            case contents M.!? subdirName' of
              Just subDir -> subDir
              Nothing -> Dir 0 subDirName M.empty
          contents' = M.insert subDirName subdir contents
          newCurDir = Dir wdSz wsName (trace ("new contents of " ++ wsName ++ ": " ++ show contents') contents')
      in go (subdir:newCurDir:ancestors) t

    go dirs cmds =
      error $ "invariant violation - unhandled interpret:\ndirs: "
        ++ show dirs ++ "\ncmds: " ++ show cmds

updateSize :: FSEntry -> FSEntry
updateSize = 
  fst . go
  where
    go :: FSEntry -> (FSEntry,Int)
    go f@(File s _) = (f, s)
    go (Dir _ name contents) =
      let
        entries = map go . M.elems $ contents
        contents' = M.fromList $ map (\(e,_) -> case e of (Dir _ n _) -> (n,e); (File _ n) -> (n,e)) entries
        sz = sum . map snd $ entries
      in (Dir sz name contents', sz)

dirSizes :: (Int -> Bool) -> FSEntry -> [Int]
dirSizes predicate root =
  go [] [root]
  where
    go acc [] = acc
    go acc ((File _ _):t) = go acc t
    go acc ((Dir sz n entries):t)
      | predicate sz =
          let sz' = trace ("counting " ++ n ++ ": " ++ show sz) sz in
          go (sz':acc) (map snd (M.toList entries) ++ t)
      | otherwise =
          let t' = trace ("skipping " ++ n ++ ": " ++ show sz) t in
          go acc (map snd (M.toList entries) ++ t')

sln :: String -> String
sln =
  show . sum . dirSizes (<= 100000) . updateSize . interpret . parseInput

totalSpace :: Int
totalSpace = 70000000

targetUnused :: Int
targetUnused = 30000000