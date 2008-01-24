-- ----------------------------------------------------------------------------

{- |
  Module     : Benchmark
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  A simple benchmark which measures the throughput of queries.

-}

-- ----------------------------------------------------------------------------

module Main where

import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.CPUTime

import Text.Printf

import qualified Data.List as L

import Holumbus.Control.Sequence

import Holumbus.Index.Common (HolIndex)

import qualified Holumbus.Index.Inverted as INV
import qualified Holumbus.Index.Hybrid as HYB
import qualified Holumbus.Index.Common as IDX

import Holumbus.Query.Syntax
import Holumbus.Query.Processor
import Holumbus.Query.Ranking
import Holumbus.Query.Result
import Holumbus.Query.Fuzzy

data Flag = Inverted String | Hybrid String | Times String | Version deriving (Show, Eq)

version :: String
version = "0.1"

main :: IO ()
main = do
       argv <- getArgs
       flags <- commandLineOpts argv
       if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
       indexes <- return (filter isIndex flags)
       times <- getTimes flags
       if L.null indexes then usage ["No index file given!\n"] else return ()
       if length indexes > 1 then usage ["Only one index file allowed!\n"] else return ()
       putStrLn "Generating queries ..."
       qs <- return (generateQueries times)
       putStrLn "Loading index ..."
       startup (head indexes) qs
       return ()

isIndex :: Flag -> Bool
isIndex (Inverted _) = True
isIndex (Hybrid _) = True
isIndex _ = False

getTimes :: [Flag] -> IO (Int)
getTimes [] = return 1
getTimes ((Times n):_) = return (read n)
getTimes ((Inverted _):fs) = getTimes fs
getTimes ((Hybrid _):fs) = getTimes fs
getTimes ((Version):fs) = getTimes fs

-- | Decide between hybrid and inverted and then fire up!
startup :: Flag -> [Query] -> IO ()
startup (Inverted file) qs = do
                             idx <- INV.loadFromXmlFile file
                             runQueries qs idx
                             
startup (Hybrid file) qs = do
                           idx <- HYB.loadFromXmlFile file
                           runQueries qs idx

startup _ _ = usage ["Internal error!\n"]
                          
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
options = [ Option ['i'] ["inverted"] (ReqArg Inverted "FILE") "Loads inverted index from FILE"
          , Option ['h'] ["hybrid"]   (ReqArg Hybrid "FILE")   "Loads hybrid index from FILE"
          , Option ['t'] ["times"]    (ReqArg Times "N")       "The number of times the queries should run"
          , Option ['V'] ["version"]  (NoArg Version)          "Output version and exit"
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

runQueries :: HolIndex i => [Query] -> i -> IO ()
runQueries qs i = do
                  printStats qs i
                  putStrLn "Running queries ..."
                  tmp1 <- strict $ getCPUTime
                  t1 <- return (fromIntegral tmp1)

                  count <- strict $ (runner qs i 0) -- Fire!
                  putStrLn $ "Total number of hits: " ++ (show count)

                  tmp2 <- strict $ getCPUTime
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

runner :: HolIndex i => [Query] -> i -> Integer -> IO (Integer)
runner [] _ a = return a
runner (q:qs) i a = do
                    l <- strict $ runQuery q i
                    runner qs i (strict $ a + (strict $ fromIntegral l))
               
runQuery :: HolIndex i => Query -> i -> IO (Int)
runQuery q i = do
               oq <- return (optimize q)
               r <- return (processQuery procCfg i oq)
               rr <- return (rank rankCfg r)
               return (strict $ (strict $ (strict $ sizeDocHits rr) + (strict $ sizeWordHits rr)))
                 where
                 procCfg = ProcessConfig [] (FuzzyConfig True True 1.0 germanReplacements)
                 rankCfg = RankConfig (docRankWeightedByCount weights) (wordRankWeightedByCount weights)
                  where
                  weights = [("title", 0.8), ("keywords", 0.6), ("headlines", 0.4), ("content", 0.2)]


printError :: String -> IO ()
printError e = usage [e]

printStats :: HolIndex i => [Query] -> i -> IO ()
printStats qs i = do
                  putStr ("Loaded " ++ (show (IDX.sizeDocs i)) ++ " documents ")
                  putStrLn ("containing " ++ show (IDX.sizeWords i) ++ " words")
                  putStrLn ("Generated " ++ (show $ length qs) ++ " queries")
