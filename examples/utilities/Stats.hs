-- ----------------------------------------------------------------------------

{- |
  Module     : Stats
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.3

  Outputs some statistics about an index and its documents.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-type-defaults  #-}

module Main where

import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt

import Control.Parallel.Strategies

import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Holumbus.Index.Inverted (InvIndex)
import Holumbus.Index.Documents (Documents)
import Holumbus.Index.Common

data Flag = Index String 
          | Documents String 
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

       idx <- return (filter isIndex flags)
       if L.null idx then usage ["No index file given!\n"] else return ()
       if length idx > 1 then usage ["Only one index file allowed!\n"] else return ()

       doc <- return (filter isDocuments flags)
       if L.null doc then usage ["No documents file given!\n"] else return ()
       if length doc > 1 then usage ["Only one documents file allowed!\n"] else return ()

       startup (head idx) (head doc)
       return ()

isIndex :: Flag -> Bool
isIndex (Index _) = True
isIndex _ = False

isDocuments :: Flag -> Bool
isDocuments (Documents _) = True
isDocuments _ = False

-- | Decide between hybrid and inverted and then fire up!
startup ::Flag -> Flag -> IO ()
startup (Index idxFile) (Documents docFile) = do
                                              putStrLn "Loading index..."
                                              idx <- (loadFromFile idxFile) :: IO InvIndex
                                              return (rnf idx)
                                              putStrLn "Loading documents..."
                                              doc <- (loadFromFile docFile) :: IO Documents
                                              return (rnf doc)
                                              printStats idx doc
startup _ _ = usage ["Internal error!\n"]

printStats :: (HolIndex i, HolDocuments d) => i -> d -> IO ()
printStats i d = do
                 putStrLn "General:"
                 putStrLn $ "Contexts:" ++ concatMap ((++) " ") (contexts i)
                 putStrLn $ "Unique documents: " ++ show noUniqueDocs
                 putStrLn $ "Unique words: " ++ show noUniqueWords
                 putStrLn $ "Documents: " ++ show noDocs
                 putStrLn $ "Words: " ++ show noWords
                 putStrLn $ "Words per document: " ++ (show $ round (fromIntegral noWords / fromIntegral noUniqueDocs))
                 putStrLn $ "Documents per word: " ++ (show $ round (fromIntegral noDocs / fromIntegral noUniqueWords))
                 printContextStats (contexts i) i
                 where
                  noUniqueDocs = sizeDocs d
                  noUniqueWords = sizeWords i
                  noDocs = L.foldl' (\r c -> L.foldl' (\n (_, o) -> n + IM.size o) r (allWords i c)) 0 (contexts i)
                  noWords = L.foldl' (\r c -> L.foldl' (\n (_, o) -> IM.fold ((+) . IS.size) n o) r (allWords i c)) 0 (contexts i)

printContextStats :: HolIndex i => [Context] -> i -> IO ()
printContextStats [] _ = return ()
printContextStats (c:cs) i = do 
                             putStrLn ""
                             putStrLn $ "Context: " ++ c
                             putStrLn $ "Unique words: " ++ (show noUniqueWords)
                             putStrLn $ "Words: " ++ (show noWords)
                             printContextStats cs i
                             where
                               noUniqueWords = length $ allWords i c
                               noWords = L.foldl' (\r (_, o) -> IM.fold ((+) . IS.size) r o) 0 (allWords i c)

usage :: [String] -> IO a
usage errs = if null errs then do
             hPutStrLn stdout use
             exitWith ExitSuccess
             else do
             hPutStrLn stderr (concat errs ++ "\n" ++ use)
             exitWith (ExitFailure (-1))
  where
  header = "Stats - Output some statistics about an index.\n\n" ++
           "Usage: Stats [OPTIONS]"
  use    = usageInfo header options

commandLineOpts :: [String] -> IO [Flag]
commandLineOpts argv = case getOpt Permute options argv of
                       (o, [], []  ) -> return o
                       (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option ['i'] ["index"] (ReqArg Index "FILE") "Loads index from FILE"
          , Option ['d'] ["documents"] (ReqArg Documents "FILE") "Loads documents from FILE"
          , Option ['V'] ["version"] (NoArg Version) "Output version and exit"
          , Option ['?'] ["help"] (NoArg Help) "Output this help and exit"
          ]
