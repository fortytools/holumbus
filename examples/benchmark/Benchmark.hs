-- ----------------------------------------------------------------------------

{- |
  Module     : Benchmark
  Copyright  : Copyright (C) 2007 Timo B. Huebel
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

import Holumbus.Index.Combined (AnyIndex(Inv, Hyb))
import qualified Holumbus.Index.Common as ANY
import qualified Holumbus.Index.Inverted as INV
import qualified Holumbus.Index.Hybrid as HYB
import qualified Holumbus.Index.Common as IDX

import Holumbus.Query.Syntax
import Holumbus.Query.Processor
import Holumbus.Query.Ranking
import Holumbus.Query.Result
import Holumbus.Query.Parser

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
       putStrLn "Loading index ..."
       idx <- load (head indexes)
       putStrLn "Generating queries ..."
       qs <- return (generateQueries times)
       runQueries qs idx
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
load :: Flag -> IO (AnyIndex)
load (Inverted file) = do
                        inv <- INV.loadFromFile file
                        return (Inv inv)
load (Hybrid file) = do
                     hyb <- HYB.loadFromFile file
                     return (Hyb hyb)
load _ = do
         usage ["Internal error!\n"]
                          
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

generateQueries :: Int -> [String]
generateQueries 0 = []
generateQueries t = allQueries ++ (generateQueries $ t - 1)
  where
  allWords = twoWords ++ oneWords
  twoWords = [ x:[y] | y <- ['a'..'z'], x <- ['a'..'z'] ]
  oneWords = [ [x] | x <- ['a'..'z']]
  allQueries = primQueries ++ unOpQueries ++ binOpQueries
  binOpQueries = [w1 ++ " " ++ w2 | w1 <- allWords, w2 <- allWords]
              ++ [w1 ++ " OR " ++ w2 | w1 <- allWords, w2 <- allWords]
              ++ [w1 ++ " NOT " ++ w2 | w1 <- allWords, w2 <- allWords]
  unOpQueries = [ "NOT " ++ w | w <- oneWords ]
  primQueries = [w | w <- allWords]
             ++ ["!" ++ w | w <- allWords]
             ++ ["\"" ++ w1 ++ " " ++ w2 ++ "\"" | w1 <- allWords, w2 <- oneWords]
             ++ ["!\"" ++ w1 ++ " " ++ w2 ++ "\"" | w1 <- allWords, w2 <- oneWords]
             ++ ["~" ++ w | w <- allWords]

runQueries :: [String] -> AnyIndex -> IO ()
runQueries qs i = do
                  printStats qs i
                  putStrLn "0%                                            100%"
                  one <- return (l / 50.0)
                  tmp1 <- getCPUTime
                  t1 <- return (fromIntegral tmp1)
                  runner qs i one 0 -- Fire!
                  tmp2 <- getCPUTime
                  t2 <- return (fromIntegral tmp2)
                  d <- return (((t2 - t1) / 1000000000000) :: Float)
                  a <- return (d / l)
                  p <- return (l / d)

                  ds <- return (printf "%.3f" d)
                  as <- return (printf "%.5f" a)
                  ps <- return (printf "%.2f" p)

                  putStrLn $ "Total running time: " ++ ds ++ " sec"
                  putStrLn $ "Seconds per query: " ++ as ++ " sec"
                  putStrLn $ "Queries per second: " ++ ps ++ " queries"
                    where
                    l = fromIntegral $ length qs

runner :: [String] -> AnyIndex -> Float -> Int -> IO ()
runner [] _ _ _ = putStrLn "="
runner (q:qs) i m c = do
                      nc <- return (if c > (floor m) then 0 else c + 1)
                      if nc == 0 then do { putStr "=" ; hFlush stdout }else return ()
                      runQuery q i
                      runner qs i m nc
               
runQuery :: String -> AnyIndex -> IO ()
runQuery s i = do
               pq <- return (parseQuery s)
               either printError makeQuery pq
  where
  printError e = usage [e]
  makeQuery q = do 
                oq <- return (optimize q)
                r <- return (process oq i (IDX.contexts i))
                rr <- return (strict $ rank r)
                return (sizeDocs rr)
                return (sizeWords rr)
                return ()

printStats :: [String] -> AnyIndex -> IO ()
printStats qs i = do
                  putStr ("Loaded " ++ (show (IDX.sizeDocs i)) ++ " documents ")
                  putStrLn ("containing " ++ show (IDX.sizeWords i) ++ " words")
                  putStrLn ("Running " ++ (show $ length qs) ++ " queries")

-- This is a fix for GHC 6.6.1 (from 6.8.1 on, this is avaliable in module Data.Function)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
op `on` f = \x y -> f x `op` f y
