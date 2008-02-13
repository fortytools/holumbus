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

import Text.Printf

import Control.Parallel.Strategies

import Char
import Data.Maybe
import Data.Function

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Text.XML.HXT.DOM.Unicode

import Holumbus.Index.Inverted (InvIndex)
import Holumbus.Index.Documents (Documents)
import Holumbus.Index.Common
import Holumbus.Query.Processor
import Holumbus.Query.Ranking
import Holumbus.Query.Fuzzy
import Holumbus.Query.Result
import Holumbus.Query.Language.Grammar
import Holumbus.Query.Language.Parser
import Holumbus.Query.Distribution.Protocol
import Holumbus.Query.Distribution.Client

data Flag = Index String 
          | Documents String 
          | Server String
          | Compress
          | Verbose 
          | Version 
          | Help deriving (Show, Eq)

version :: String
version = "0.5"

main :: IO ()
main = 
  do
  argv <- getArgs
  flags <- commandLineOpts argv

  if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
  if Help `elem` flags then usage [] >> (exitWith ExitSuccess) else return ()

  verbose <- return (Verbose `elem` flags)
  compress <- return (Compress `elem` flags)

  doc <- return (filter isDocuments flags)
  if L.null doc then usage ["No documents file given!\n"] else return ()
  if length doc > 1 then usage ["Only one documents file allowed!\n"] else return ()

  idx <- return (filter isIndex flags)
  srv <- return (filter isServer flags)

  if not (L.null idx) && not (L.null srv) then usage ["Cannot use local index and remote index at the same time!\n"] else return ()

  if L.null idx then do
    if L.null srv then usage ["No query server specified!\n"] else return ()
     
    startupDistributed verbose (head doc) (map fromServer srv) compress
    else do
      if L.null idx then usage ["No index file given!\n"] else return ()
      if length idx > 1 then usage ["Only one index file allowed!\n"] else return ()

      if compress then usage ["Compression not avaliable for local index!\n"] else return ()

      startupLocal verbose (head idx) (head doc)

isIndex :: Flag -> Bool
isIndex (Index _) = True
isIndex _ = False

isDocuments :: Flag -> Bool
isDocuments (Documents _) = True
isDocuments _ = False

isServer :: Flag -> Bool
isServer (Server _) = True
isServer _ = False

fromServer :: Flag -> Server
fromServer (Server s) = s
fromServer _ = ""

-- | Walk through the whole index to remove laziness.
walkIndex :: HolIndex i => i -> Integer
walkIndex i = L.foldl' (\r c -> L.foldl' sumPositions r (allWords i c)) 0 (contexts i)
  where
  sumPositions r (_, o) = IM.fold (\d r' -> IS.fold (\p r'' -> r'' + fromIntegral p) r' d) r o
  
-- | Startup using local index.
startupLocal :: Bool -> Flag -> Flag -> IO ()
startupLocal v (Index idxFile) (Documents docFile) = 
  do
  putStrLn "Loading index..."
  idx <- (loadFromFile idxFile) :: IO InvIndex
  return (rnf idx)
  if v  then putStrLn ("Collected position total of " ++ (show $ walkIndex idx)) else return ()
  putStrLn ("Loaded " ++ show (sizeWords idx) ++ " words")
  putStrLn "Loading documents..."
  doc <- (loadFromFile docFile) :: IO Documents
  return (rnf doc)
  putStrLn ("Loaded " ++ show (sizeDocs doc) ++ " documents ")
  answerQueries (localQuery idx doc) v
startupLocal _ _ _ = usage ["Internal error!\n"]

-- | Startup using remote index.
startupDistributed :: Bool -> Flag -> [Server] -> Bool -> IO ()
startupDistributed v (Documents docFile) srvs compr = 
  do
  putStrLn "Loading documents..."
  doc <- (loadFromFile docFile) :: IO Documents
  return (rnf doc)
  putStrLn ("Loaded " ++ show (sizeDocs doc) ++ " documents ")
  answerQueries (distributedQuery doc srvs compr) v
startupDistributed _ _ _ _ = usage ["Internal error!\n"]                     

-- | Create the configuration for the query processor.
processCfg :: ProcessConfig
processCfg = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True

-- | Perform a query on a local index.
localQuery :: (HolIndex i, HolDocuments d) => i -> d -> Query -> IO Result
localQuery i d q = return (processQuery processCfg i d q)

-- | Perform a query on a remote index.
distributedQuery :: HolDocuments d => d -> [Server] -> Bool -> Query -> IO Result
distributedQuery d s c q = processDistributed cfg d q
  where
  cfg = DistributedConfig s c processCfg
                          
usage :: [String] -> IO a
usage errs = 
  if L.null errs then do
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
commandLineOpts argv = 
  case getOpt Permute options argv of
  (o, [], []  ) -> return o
  (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option "i" ["index"] (ReqArg Index "FILE") "Loads index from FILE"
          , Option "d" ["documents"] (ReqArg Documents "FILE") "Loads documents from FILE"
          , Option "s" ["server"] (ReqArg Server "HOST") "Distribute queries using HOST"
          , Option "c" ["compress"] (NoArg Compress) "Use compression for transmitting results"
          , Option "v" ["verbose"] (NoArg Verbose) "Be more verbose"
          , Option "V" ["version"] (NoArg Version) "Output version and exit"
          , Option "?" ["help"] (NoArg Help) "Output this help and exit"
          ]

answerQueries ::(Query -> IO Result) -> Bool -> IO ()
answerQueries f verbose = 
  do
  q <- readline ("Enter query (type :? for help) > ")
  if isNothing q then answerQueries f verbose else
    do
    n <- return (fst $ utf8ToUnicode (fromJust q))
    addHistory n
    answerQueries' n
      where
      answerQueries' :: String -> IO ()
      answerQueries' ""       = answerQueries f verbose
      answerQueries' (':':xs) = 
        do
        internalCommand xs
        answerQueries f verbose
      answerQueries' q = 
        do
        pr <- return (parseQuery q)
        if verbose then putStrLn ("Query: \n" ++ (show pr) ++ "\n") else return ()
        either printError makeQuery pr
        answerQueries f verbose
          where
          printError err = putStrLn ("Problem parsing query: " ++ err)
          makeQuery pq = 
            do
            t1 <- getCPUTime
            r <- f pq -- This is where the magic happens!
            rr <- return (rank rankCfg r)

            printDocHits (docHits rr)
            putStrLn ""
            printWordHits (wordHits rr)
            t2 <-  getCPUTime
            
            d <- return ((fromIntegral (t2 - t1) / 1000000000000) :: Float)

            ds <- return (printf "%.4f" d)
            putStrLn ""
            putStrLn ("Query processed in " ++ ds ++ " sec")
              where
              rankCfg = RankConfig (docRankWeightedByCount weights) (wordRankWeightedByCount weights)
               where
               weights = [("title", 0.8), ("keywords", 0.6), ("headlines", 0.4), ("content", 0.2)]

internalCommand :: String -> IO ()
internalCommand "q" = exitWith ExitSuccess
internalCommand "?" = 
  do
  putStrLn ""
  printHelp
internalCommand _   = 
  do
  putStrLn "Unknown command!"

printDocHits :: DocHits -> IO ()
printDocHits h = 
  do
  putStrLn "Result:"
  printHits' (L.sortBy (compare `on` (docScore . fst . snd)) $ IM.toList h)
  putStrLn ""
  putStrLn ("Found " ++ (show (IM.size h)) ++ " documents")
    where
      printHits' [] = return ()
      printHits' ((_, (di, _)):xs) = 
        do
        putStr (fst $ document di)
        putStr " Score: "
        putStrLn (show $ docScore  di)
        putStrLn (snd $ document di)
        printHits' xs
        return ()

printWordHits :: WordHits -> IO ()
printWordHits h = 
  do
  putStrLn "Completions:"
  d <- return (L.sortBy (compare `on` snd) (map (\(c, (_, o)) -> (c, M.fold (\m r -> r + IM.size m) 0 o)) (M.toList h)))
  putStrLn (foldr (\(c, s) r -> r ++ c ++ " (" ++ (show s) ++ ") ") "" d)
  putStrLn ""
  putStrLn ("Found " ++ (show (M.size h)) ++ " possible completions")

printHelp :: IO ()
printHelp = 
  do
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
