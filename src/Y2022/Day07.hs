{- DAY07 : https://adventofcode.com/2022/day/7 -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2022.Day07 (sln07A, sln07B) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Util (startsWith)

-- | Directory entries is a map from the directory name to the @FSEntry@
type DirEntries = M.Map String FSEntry

{-| An entry in the file system, either a file with size and name, or a 
    directory with size and name and a map to the contents (@DirEntries@) -}
data FSEntry
  = File Int String
  | Dir Int String DirEntries
  deriving (Show, Eq, Ord)

{-| A command is the simulation of a CLI command typed at the terminal 
    prompt. -}
data Command
  = CD String   -- ^ Change sub-directory 
  | UP          -- ^ Change to parent directory
  | LS          -- ^ List the contents of current directory
  deriving (Show, Eq)

{-| represents a unit of syntaxed parsed out of the raw input -}
data Lexeme
  = Cmd Command     -- ^ A command typed at the terminal prompt
  | FSE FSEntry     -- ^ A file system entry output by the simulated OS
  deriving (Show, Eq)

{-| parse the input string into a list of lexemes -}
parse :: String -> [Lexeme]
parse = map parseLine . lines

{-| parse a single line from the input string into a Lexeme. In this
    syntax, there's a 1:1 relation between lines of input and lexemes. 
    Note we skip tokenization and translate raw input directly
    into the lexemes they represent. -}
parseLine :: String -> Lexeme
parseLine [] = error "end of input"
parseLine s
  | s `startsWith` "$ cd .." = Cmd UP
  | s `startsWith` "$ cd "   = Cmd (CD (drop 5 s))
  | s `startsWith` "$ ls"    = Cmd LS
  | s `startsWith` "dir "    = FSE (Dir 0 (drop 4 s) M.empty)
  | otherwise                =
    let [rawSize, filename] = splitOn " " s in
    FSE (File (read rawSize) filename)

{-| interprets the lexemes into the @FSEntry@ for the root of the file
    system -> a tree that contains all the information on this file system -}
interpret :: [Lexeme] -> FSEntry
interpret = go []
  where
    go :: [FSEntry] -> [Lexeme] -> FSEntry
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

{-| parses lexemes into dirctory entries and adds them to the
    map of directory entries, recurses down the list of
    lexemes adding entries to sub directories as needed. -}
addEntries :: DirEntries -> [Lexeme] -> (DirEntries, [Lexeme])
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

{-| given an @FSEntry@, updates the size of the entry. For files,
    the size is already in place as a result of parsing, for
    directories, the sizes of the contents of the directory is 
    summed, including recursively updating the size of sub dirs.
    
    This is done as a second pass over the directory tree because
    we can't know for sure that we have the complete directory
    tree for any entry until we've parsed and interpreted 
    the entire input string -}
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

{-| Collects the directory sizes as a list, filtering by a predicate
    that allows us to answer the puzzle questions -}
dirSizes :: (Int -> Bool) -> FSEntry -> [Int]
dirSizes predicate root =
  go [] [root]
  where
    go acc [] = acc
    go acc ((File _ _):t) = go acc t
    go acc ((Dir sz _ entries):t)
      | predicate sz = go (sz:acc) (map snd (M.toList entries) ++ t)
      | otherwise    = go acc (map snd (M.toList entries) ++ t)

{-- Solutions ----------------------------------------------------- 
    Standard wrappers that can be leveraged by @Util.solve@ 
-------------------------------------------------------------------}

{-| The first part gets the sum of directory sizes that are less than
    100k size units -}
sln07A :: String -> String
sln07A =
  show . sum . dirSizes (<= 100000) . updateSize . interpret . parse

{-| The second part finds the smallest single directory that would have
    to be deleted in order for the file system to reach the target
    unused space figure -}
sln07B :: String -> String
sln07B s =
  let
    totalSpace        = 70000000
    targetUnused      = 30000000
    root@(Dir sz _ _) = updateSize . interpret . parse $ s
    available         = totalSpace - sz
    reqd              = targetUnused - available
    sizes             = dirSizes (const True) root in
  show . minimum . filter (>= reqd) $ sizes