-- ----------------------------------------------------------------------------

{- |
  Module     : Benchmark
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

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

data Flag = Index String 
          | Documents String 
          | Times String 
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

       times <- getTimes flags

       idx <- return (filter isIndex flags)
       if L.null idx then usage ["No index file given!\n"] else return ()
       if length idx > 1 then usage ["Only one index file allowed!\n"] else return ()

       doc <- return (filter isDocuments flags)
       if L.null doc then usage ["No documents file given!\n"] else return ()
       if length doc > 1 then usage ["Only one documents file allowed!\n"] else return ()

       putStrLn "Generating queries ..."
       qs <- return (generateQueries times)

       startup qs (head idx) (head doc)
       return ()

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

-- | Decide between hybrid and inverted and then fire up!
startup :: [Query] -> Flag -> Flag -> IO ()
startup qs (Index idxFile) (Documents docFile) = do
                                                 putStrLn "Loading index..."
                                                 idx <- (loadFromFile idxFile) :: IO InvIndex
                                                 return (rnf idx)
                                                 putStrLn "Loading documents..."
                                                 doc <- (loadFromFile docFile) :: IO Documents
                                                 return (rnf doc)
                                                 printStats qs idx doc
                                                 runQueries qs idx doc
startup _ _ _ = usage ["Internal error!\n"]
                          
usage :: [String] -> IO a
usage errs = if L.null errs then do
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
commandLineOpts argv = case getOpt Permute options argv of
                       (o, [], []  ) -> return o
                       (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option ['i'] ["index"] (ReqArg Index "FILE") "Loads index from FILE"
          , Option ['d'] ["documents"] (ReqArg Documents "FILE") "Loads documents from FILE"
          , Option ['t'] ["times"] (ReqArg Times "N") "The number of times the queries should run"
          , Option ['V'] ["version"] (NoArg Version) "Output version and exit"
          , Option ['?'] ["help"] (NoArg Help) "Output this help and exit"
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

runQueries :: (HolIndex i, HolDocuments d) => [Query] -> i -> d -> IO ()
runQueries qs i d = do
                    putStrLn "Running queries ..."
                    tmp1 <-getCPUTime
                    t1 <- return (fromIntegral tmp1)
  
                    count <- (runner qs i d 0) -- Fire!
                    putStrLn $ "Total number of hits: " ++ (show count)
  
                    tmp2 <- getCPUTime
                    t2 <- return (fromIntegral tmp2)
                    f <- return (((t2 - t1) / 1000000000000) :: Float)
                    a <- return (f / l)
                    p <- return (l / f)
  
                    fs <- return (printf "%.3f" f)
                    as <- return (printf "%.4f" a)
                    ps <- return (printf "%.2f" p)
  
                    putStrLn $ "Number of executed queries: " ++ (show $ floor l)
                    putStrLn $ "Total running time: " ++ fs ++ " sec"
                    putStrLn $ "Seconds per query: " ++ as ++ " sec"
                    putStrLn $ "Queries per second: " ++ ps ++ " queries"
                      where
                      l = fromIntegral $ length qs

runner :: (HolIndex i, HolDocuments d) => [Query] -> i -> d -> Integer -> IO (Integer)
runner [] _ _ a = return a
runner (q:qs) i d a = do
                      l <- runQuery q i d
                      runner qs i d (a + fromIntegral l)
               
runQuery :: (HolIndex i, HolDocuments d) => Query -> i -> d -> IO (Int)
runQuery q i d = do
                 oq <- return (optimize q)
                 r <- return (processQuery procCfg i d oq)
                 rr <- return (rank rankCfg r)
                 return (sizeDocHits rr + sizeWordHits rr)
                   where
                   procCfg = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements)
                   rankCfg = RankConfig (docRankWeightedByCount weights) (wordRankWeightedByCount weights)
                     where
                     weights = [("title", 0.8), ("keywords", 0.6), ("headlines", 0.4), ("content", 0.2)]


printError :: String -> IO ()
printError e = usage [e]

printStats :: (HolIndex i, HolDocuments d) => [Query] -> i -> d -> IO ()
printStats qs i d = do
                    putStr ("Loaded " ++ (show (sizeDocs d)) ++ " documents ")
                    putStrLn ("containing " ++ show (sizeWords i) ++ " words")
                    putStrLn ("Generated " ++ (show $ length qs) ++ " queries")
