-- ----------------------------------------------------------------------------

{- |
  Module     : Words
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Outputs all unique words of an index.

-}

-- ----------------------------------------------------------------------------

module Main where

import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt

import Control.Parallel.Strategies

import qualified Data.List as L

import Text.XML.HXT.DOM.Unicode

import Holumbus.Index.Inverted (InvIndex)
import Holumbus.Index.Documents (Documents)
import Holumbus.Index.Common

data Flag = Index String 
          | Documents String 
          | Con Context 
          | Version 
          | Help deriving (Show, Eq)

version :: String
version = "0.2"

main :: IO ()
main = do
       argv <- getArgs
       flags <- commandLineOpts argv

       if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
       if Help `elem` flags then usage [] >> (exitWith ExitSuccess) else return ()

       ctx <- return (filter isContext flags)
       if null ctx then usage ["No context given!\n"] else return ()

       idx <- return (filter isIndex flags)
       if L.null idx then usage ["No index file given!\n"] else return ()
       if length idx > 1 then usage ["Only one index file allowed!\n"] else return ()

       doc <- return (filter isDocuments flags)
       if L.null doc then usage ["No documents file given!\n"] else return ()
       if length doc > 1 then usage ["Only one documents file allowed!\n"] else return ()

       startup (map fromContext ctx) (head idx) (head doc)
       return ()

isContext :: Flag -> Bool
isContext (Con _) = True
isContext _ = False

isIndex :: Flag -> Bool
isIndex (Index _) = True
isIndex _ = False

isDocuments :: Flag -> Bool
isDocuments (Documents _) = True
isDocuments _ = False

fromContext :: Flag -> Context
fromContext (Con s) = s
fromContext _ = ""

-- | Decide between hybrid and inverted and then fire up!
startup :: [Context] -> Flag -> Flag -> IO ()
startup c (Index idxFile) (Documents docFile) = do
                                                putStrLn "Loading index..."
                                                idx <- (loadFromFile idxFile) :: IO InvIndex
                                                return (rnf idx)
                                                putStrLn "Loading documents..."
                                                doc <- (loadFromFile docFile) :: IO Documents
                                                return (rnf doc)
                                                printWords c idx doc
startup _ _ _ = usage ["Internal error!\n"]

printWords :: (HolIndex i, HolDocuments d) => [Context] -> i -> d -> IO ()
printWords [] _ _ = exitWith ExitSuccess
printWords (c:cs) i d = do
                        printWords' c
                        printWords cs i d
  where
  printWords' c' = do
                    w <- return (concat $ L.sort $ map (\(w, _) -> unicodeToUtf8 w ++ "\n") (allWords i c'))
                    putStrLn w

usage :: [String] -> IO a
usage errs = if null errs then do
             hPutStrLn stdout use
             exitWith ExitSuccess
             else do
             hPutStrLn stderr (concat errs ++ "\n" ++ use)
             exitWith (ExitFailure (-1))
  where
  header = "Words - Output all unique words of an index.\n\n" ++
           "Usage: Words [OPTIONS]"
  use    = usageInfo header options

commandLineOpts :: [String] -> IO [Flag]
commandLineOpts argv = case getOpt Permute options argv of
                       (o, [], []  ) -> return o
                       (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option ['i'] ["index"]     (ReqArg Index "FILE")     "Loads index from FILE"
          , Option ['d'] ["documents"] (ReqArg Documents "FILE") "Loads documents from FILE"
          , Option ['c'] ["context"]   (ReqArg Con "CONTEXT")    "Show words from context CONTEXT"
          , Option ['V'] ["version"]   (NoArg Version)           "Output version and exit"
          , Option ['?'] ["help"]      (NoArg Help)           "Output this help and exit"
          ]
