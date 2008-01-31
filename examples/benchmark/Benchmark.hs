-- ----------------------------------------------------------------------------

{- |
  Module     : Benchmark
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.3

  A simple benchmark which measures the throughput of queries.

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-type-defaults  #-}

module Main where

import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.CPUTime

import Text.Printf

import Control.Parallel.Strategies

import qualified Data.List as L

import Holumbus.Index.Inverted (InvIndex)
import Holumbus.Index.Documents (Documents)
import Holumbus.Index.Common
import Holumbus.Query.Language
import Holumbus.Query.Processor
import Holumbus.Query.Ranking
import Holumbus.Query.Fuzzy
import Holumbus.Query.Result
import Holumbus.Query.Distribution

data Flag = Index String 
          | Documents String 
          | Server String
          | Compress
          | Times String 
          | Version 
          | Help deriving (Show, Eq)

version :: String
version = "0.3"

main :: IO ()
main = 
  do
  argv <- getArgs
  flags <- commandLineOpts argv

  if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
  if Help `elem` flags then usage [] >> (exitWith ExitSuccess) else return ()

  times <- getTimes flags
  compress <- return (Compress `elem` flags)

  putStrLn "Generating queries ..."
  qs <- return (generateQueries times)
  putStrLn ("Generated " ++ (show $ length qs) ++ " queries")

  doc <- return (filter isDocuments flags)
  if L.null doc then usage ["No documents file given!\n"] else return ()
  if length doc > 1 then usage ["Only one documents file allowed!\n"] else return ()

  idx <- return (filter isIndex flags)
  srv <- return (filter isServer flags)

  if not (L.null idx) && not (L.null srv) then usage ["Cannot use local index and remote index at the same time!\n"] else return ()

  if L.null idx then do
    if L.null srv then usage ["No query server specified!\n"] else return ()
     
    startupDistributed qs (head doc) (map fromServer srv) compress
    else do
      if L.null idx then usage ["No index file given!\n"] else return ()
      if length idx > 1 then usage ["Only one index file allowed!\n"] else return ()

      if compress then usage ["Compression not avaliable for local index!\n"] else return ()

      startupLocal qs (head idx) (head doc)

isIndex :: Flag -> Bool
isIndex (Index _) = True
isIndex _ = False

isDocuments :: Flag -> Bool
isDocuments (Documents _) = True
isDocuments _ = False

getTimes :: [Flag] -> IO (Int)
getTimes [] = return 1
getTimes ((Times n):_) = return (read n)
getTimes (_:fs) = getTimes fs

isServer :: Flag -> Bool
isServer (Server _) = True
isServer _ = False

fromServer :: Flag -> Server
fromServer (Server s) = s
fromServer _ = ""

-- | Startup using local index.
startupLocal :: [Query] -> Flag -> Flag -> IO ()
startupLocal qs (Index idxFile) (Documents docFile) = 
  do
  putStrLn "Loading index..."
  idx <- (loadFromFile idxFile) :: IO InvIndex
  return (rnf idx)
  putStrLn ("Loaded " ++ show (sizeWords idx) ++ " words")
  putStrLn "Loading documents..."
  doc <- (loadFromFile docFile) :: IO Documents
  return (rnf doc)
  putStrLn ("Loaded " ++ show (sizeDocs doc) ++ " documents ")
  runQueries (localQuery idx doc) qs
startupLocal _ _ _ = usage ["Internal error!\n"]

-- | Startup using remote index.
startupDistributed :: [Query] -> Flag -> [Server] -> Bool -> IO ()
startupDistributed qs (Documents docFile) srvs compr = 
  do
  putStrLn "Loading documents..."
  doc <- (loadFromFile docFile) :: IO Documents
  return (rnf doc)
  putStrLn ("Loaded " ++ show (sizeDocs doc) ++ " documents ")
  runQueries (distributedQuery doc srvs compr) qs
startupDistributed _ _ _ _ = usage ["Internal error!\n"]                     

-- | Perform a query on a local index.
localQuery :: (HolIndex i, HolDocuments d) => i -> d -> Query -> IO Result
localQuery i d q = return (processQuery cfg i d q)
  where
  cfg = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements)

-- | Perform a query on a remote index.
distributedQuery :: HolDocuments d => d -> [Server] -> Bool -> Query -> IO Result
distributedQuery d s c q = processDistributed cfg d q
  where
  cfg = DistributedConfig s c
                          
usage :: [String] -> IO a
usage errs = 
  if L.null errs then do
  hPutStrLn stdout use
  exitWith ExitSuccess
  else do
  hPutStrLn stderr (concat errs ++ "\n" ++ use)
  exitWith (ExitFailure (-1))
    where
    header = "Benchmark - A simple benchmark measuring query throughput.\n\n" ++
             "Usage: Benchmark [OPTIONS]"
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
          , Option "t" ["times"] (ReqArg Times "N") "The number of times the queries should run"
          , Option "V" ["version"] (NoArg Version) "Output version and exit"
          , Option "?" ["help"] (NoArg Help) "Output this help and exit"
          ]

generateQueries :: Int -> [Query]
generateQueries 0 = []
generateQueries t = allQueries ++ (generateQueries $ t - 1)
  where
  twoWords = [ x:[y] | y <- ['a'..'z'], x <- ['a'..'z'] ]
  oneWords = [ [x] | x <- ['a'..'z']]
  allQueries = primQueries ++ unOpQueries ++ binOpQueries
  binOpQueries = [BinQuery And (Word w1) (Word w2) | w1 <- oneWords, w2 <- oneWords]
              ++ [BinQuery Or (Word w1) (Word w2) | w1 <- oneWords, w2 <- oneWords]
              ++ [BinQuery And (Word w1) (Negation (Word w2)) | w1 <- oneWords, w2 <- oneWords]
  unOpQueries = [Negation (Word w) | w <- oneWords ]
  primQueries = [Word w | w <- oneWords]
             ++ [CaseWord w | w <- oneWords]
             ++ [Phrase (w1 ++ " " ++ w2) | w1 <- oneWords, w2 <- oneWords]
             ++ [CasePhrase (w1 ++ " " ++ w2) | w1 <- oneWords, w2 <- oneWords]
             ++ [FuzzyWord w | w <- twoWords]

runQueries ::(Query -> IO Result) -> [Query] -> IO ()
runQueries f qs = 
  do
  putStrLn "Running queries ..."
  tmp1 <-getCPUTime
  t1 <- return (fromIntegral tmp1)

  count <- (runner f qs 0) -- Fire!
  putStrLn $ "Total number of hits: " ++ (show count)

  tmp2 <- getCPUTime
  t2 <- return (fromIntegral tmp2)
  d <- return (((t2 - t1) / 1000000000000) :: Float)
  a <- return (d / l)
  p <- return (l / d)

  ds <- return (printf "%.3f" d)
  as <- return (printf "%.4f" a)
  ps <- return (printf "%.2f" p)

  putStrLn $ "Number of executed queries: " ++ (show $ floor l)
  putStrLn $ "Total running time: " ++ ds ++ " sec"
  putStrLn $ "Seconds per query: " ++ as ++ " sec"
  putStrLn $ "Queries per second: " ++ ps ++ " queries"
    where
    l = fromIntegral $ length qs

runner :: (Query -> IO Result) -> [Query] -> Integer -> IO (Integer)
runner _ [] a = return a
runner f (q:qs) a = 
  do
  l <- runQuery f q
  runner f qs (a + fromIntegral l)
               
runQuery :: (Query -> IO Result) -> Query -> IO (Int)
runQuery f q = 
  do
  r <- f q -- This is where the magic happens!
  rr <- return (rank rankCfg r)
  return (sizeDocHits rr + sizeWordHits rr)
    where
    rankCfg = RankConfig (docRankWeightedByCount weights) (wordRankWeightedByCount weights)
      where
      weights = [("title", 0.8), ("keywords", 0.6), ("headlines", 0.4), ("content", 0.2)]

printError :: String -> IO ()
printError e = usage [e]

