-- ----------------------------------------------------------------------------

{- |
  Module     : SimpleSearch
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.5

  A simple example of Holumbus, providing a command line search with the
  default query language.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-type-defaults  #-}

module Main where

import System.IO
import System.Environment
import System.Exit
import System.Console.Readline
import System.Console.GetOpt
import System.CPUTime

import Control.Parallel.Strategies

import Char
import Data.Maybe

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Text.XML.HXT.DOM.Unicode

import Holumbus.Index.Inverted (InvIndex)
import Holumbus.Index.Documents (Documents)
import Holumbus.Index.Common
import Holumbus.Query.Syntax
import Holumbus.Query.Parser
import Holumbus.Query.Processor
import Holumbus.Query.Ranking
import Holumbus.Query.Fuzzy
import Holumbus.Query.Result

data Flag = Index String 
          | Documents String 
          | Verbose 
          | Version 
          | Help deriving (Show, Eq)

version :: String
version = "0.5"

main :: IO ()
main = do
       argv <- getArgs
       flags <- commandLineOpts argv

       if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
       if Help `elem` flags then usage [] >> (exitWith ExitSuccess) else return ()

       verbose <- return (Verbose `elem` flags)

       idx <- return (filter isIndex flags)
       if L.null idx then usage ["No index file given!\n"] else return ()
       if length idx > 1 then usage ["Only one index file allowed!\n"] else return ()

       doc <- return (filter isDocuments flags)
       if L.null doc then usage ["No documents file given!\n"] else return ()
       if length doc > 1 then usage ["Only one documents file allowed!\n"] else return ()
           
       startup verbose (head idx) (head doc)
       return ()

isIndex :: Flag -> Bool
isIndex (Index _) = True
isIndex _ = False

isDocuments :: Flag -> Bool
isDocuments (Documents _) = True
isDocuments _ = False

-- | Decide between hybrid and inverted and then fire up!
startup :: Bool -> Flag -> Flag -> IO ()
startup v (Index idxFile) (Documents docFile) = do
                                                putStrLn "Loading index..."
                                                idx <- (loadFromFile idxFile) :: IO InvIndex
                                                return (rnf idx)
                                                putStrLn "Loading documents..."
                                                doc <- (loadFromFile docFile) :: IO Documents
                                                return (rnf doc)
                                                printStats idx doc
                                                answerQueries v idx doc
startup _ _ _ = usage ["Internal error!\n"]
                          
usage :: [String] -> IO a
usage errs = if L.null errs then do
             hPutStrLn stdout use
             exitWith ExitSuccess
             else do
             hPutStrLn stderr (concat errs ++ "\n" ++ use)
             exitWith (ExitFailure (-1))
  where
  header = "SimpleSearch - A simple command-line search using the Holumbus library.\n\n" ++
           "Usage: SimpleSearch [OPTIONS]"
  use    = usageInfo header options

commandLineOpts :: [String] -> IO [Flag]
commandLineOpts argv = case getOpt Permute options argv of
                       (o, [], []  ) -> return o
                       (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option "i" ["index"] (ReqArg Index "FILE") "Loads index from FILE"
          , Option "d" ["documents"] (ReqArg Documents "FILE") "Loads documents from FILE"
          , Option "v" ["verbose"] (NoArg Verbose) "Be more verbose"
          , Option "V" ["version"] (NoArg Version) "Output version and exit"
          , Option "?" ["help"] (NoArg Help) "Output this help and exit"
          ]

answerQueries :: (HolIndex i, HolDocuments d) => Bool -> i -> d -> IO ()
answerQueries verbose i d = do
                            q <- readline ("Enter query (type :? for help) > ")
                            if isNothing q then answerQueries verbose i d else
                              do
                              n <- return (fst $ utf8ToUnicode (fromJust q))
                              addHistory n
                              answerQueries' n
  where
  answerQueries' :: String -> IO ()
  answerQueries' ""       = answerQueries verbose i d
  answerQueries' (':':xs) = internalCommand verbose i d xs
  answerQueries' q        = do
                            pr <- return (parseQuery q)
                            if verbose then putStrLn ("Query: \n" ++ (show pr) ++ "\n") else return ()
                            either printError makeQuery pr
                            answerQueries verbose i d
    where
    printError err = putStrLn ("Problem parsing query: " ++ err)
    makeQuery pq = do
                   t1 <- getCPUTime
                   oq <- return (optimize pq)
                   if verbose then putStrLn ("Optimized: \n" ++ (show oq) ++ "\n") else return ()
                   r <- return (processQuery procCfg i d oq)
                   rr <- return (rank rankCfg r)
                   printDocHits (docHits rr)
                   putStrLn ""
                   printWordHits (wordHits rr)
                   t2 <- getCPUTime
                   s <- return (show $ round $ (fromIntegral $ (t2 - t1)) / 1000000000000)
                   m <- return (show $ round $ (fromIntegral $ (t2 - t1)) / 1000000000)
                   putStrLn ""
                   putStrLn ("Query processed in " ++ s ++ "." ++ m ++ " sec")
                     where
                     procCfg = ProcessConfig [] (FuzzyConfig True True 1.0 germanReplacements)
                     rankCfg = RankConfig (docRankWeightedByCount weights) (wordRankWeightedByCount weights)
                      where
                      weights = [("title", 0.8), ("keywords", 0.6), ("headlines", 0.4), ("content", 0.2)]

internalCommand :: (HolIndex i, HolDocuments d) => Bool -> i -> d -> String -> IO ()
internalCommand _       _ _ "q"       = exitWith ExitSuccess
internalCommand verbose i d "?"       = do
                                        putStrLn ""
                                        printHelp
                                        putStrLn ""
                                        printContexts i
                                        putStrLn ""
                                        answerQueries verbose i d
internalCommand verbose i d _         = do
                                        putStrLn "Unknown command!"
                                        answerQueries verbose i d

printDocHits :: DocHits -> IO ()
printDocHits h = do
                 putStrLn "Result:"
                 printHits' (L.sortBy (compare `on` (docScore . fst . snd)) $ IM.toList h)
                 putStrLn ""
                 putStrLn ("Found " ++ (show (IM.size h)) ++ " documents")
                 where
                   printHits' [] = return ()
                   printHits' ((_, (di, _)):xs) = do
                                                  putStr (fst $ document di)
                                                  putStr " Score: "
                                                  putStrLn (show $ docScore  di)
                                                  putStrLn (snd $ document di)
                                                  printHits' xs
                                                  return ()

printWordHits :: WordHits -> IO ()
printWordHits h = do
                  putStrLn "Completions:"
                  d <- return (L.sortBy (compare `on` snd) (map (\(c, (_, o)) -> (c, M.fold (\m r -> r + IM.size m) 0 o)) (M.toList h)))
                  putStrLn (foldr (\(c, s) r -> r ++ c ++ " (" ++ (show s) ++ ") ") "" d)
                  putStrLn ""
                  putStrLn ("Found " ++ (show (M.size h)) ++ " possible completions")

printHelp :: IO ()
printHelp = do
            putStrLn "Holumbus treats single words as prefix terms and will give you possible completions."
            putStrLn "Words are interpreted case insensitive. Phrases and exact matches (case sensitive)"
            putStrLn "can be specified by using quotes (i.e. \"Foo Bar\" will match this exact sequence)."
            putStrLn "Terms just separated by space will be treated implicitly as AND terms."
            putStrLn "Other operators have to be specified explicitly. Avaliable operators are: AND, OR, NOT"
            putStrLn "Priority can be influenced by round parantheses. If unsure about spelling, a single"
            putStrLn "word can be preceeded by ~ to make a fuzzy query."
            putStrLn "The contexts to search can be restricted with the : operator (seperate them with , )."
            putStrLn "Example: firstcontext,secondcontext:(foo OR bar) NOT foobar"
            putStrLn "This will search for documents containing \"foo\" or \"bar\" in the contexts named"
            putStrLn "\"firstcontext\" and \"secondcontext\" and no \"foobar\" in the all contexts."
            putStrLn ""
            putStrLn "Use :q to exit and :? to show this help."
            return ()

printContexts :: HolIndex i => i -> IO ()
printContexts i = do
                  putStrLn "Avaliable contexts:"
                  printContexts' (contexts i)
                  where
                    printContexts' :: [String] -> IO ()
                    printContexts' [] = return ()
                    printContexts' (x:xs) = do
                                            putStrLn x
                                            printContexts' xs
                                            return ()

printStats :: (HolIndex i, HolDocuments d) => i -> d -> IO ()
printStats i d = do
                 putStr ("Loaded " ++ show (sizeDocs d) ++ " documents ")
                 putStrLn ("containing " ++ show (sizeWords i) ++ " words")

-- This is a fix for GHC 6.6.1 (from 6.8.1 on, this is avaliable in module Data.Function)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
op `on` f = \x y -> f x `op` f y
