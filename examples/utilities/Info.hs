-- ----------------------------------------------------------------------------

{- |
  Module     : Info
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Outputs some statistics about an an index. This includes 
  highly specific data depending on the index type which can be 
  used for further optimizing. Right now, only 'InvIndex' is
  supported.

  For general statistics about an index, see the Stats program. 

-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-type-defaults #-}

module Main where

import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt

import Control.Parallel.Strategies

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM

import qualified Holumbus.Data.StrMap as SM
import qualified Holumbus.Data.DiffList as DL

import Holumbus.Index.Inverted (InvIndex (..))
import Holumbus.Index.Common

data Flag = Index String 
          | Version 
          | Help deriving (Show, Eq)

version :: String
version = "0.1"

main :: IO ()
main = do
       argv <- getArgs
       flags <- commandLineOpts argv

       if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
       if Help `elem` flags then usage [] >> (exitWith ExitSuccess) else return ()

       idx <- return (filter isIndex flags)
       if L.null idx then usage ["No index file given!\n"] else return ()
       if length idx > 1 then usage ["Only one index file allowed!\n"] else return ()

       startup (head idx)
       return ()

isIndex :: Flag -> Bool
isIndex (Index _) = True
isIndex _ = False

startup ::Flag -> IO ()
startup (Index idxFile) = do
                          putStrLn "Loading index..."
                          idx <- (loadFromFile idxFile) :: IO InvIndex
                          return (rnf idx)
                          printInvIndexStats idx
startup _ = usage ["Internal error!\n"]

printInvIndexStats :: InvIndex -> IO ()
printInvIndexStats (InvIndex parts) = 
  do
  totals <- printContextStats (M.toList parts) ([], [])
  putStrLn "=== Total: ===\n"
  putStrLn "Trie stats:"
  outputStats (L.sort $ fst totals)
  putStrLn "\nDiffList stats:"
  outputStats (L.sort $ snd totals)
    where
    printContextStats [] t = return t
    printContextStats ((c, w):xs) t =
      do
      putStrLn ("=== " ++ c ++ ": ===\n")
      tl <- return (L.sort $ SM.lengths w)
      dd <- return (L.sort $ map fromIntegral $ SM.fold (flip $ IM.fold (\p r -> DL.diffs p ++ r)) [] w)
      putStrLn "Trie stats:"
      outputStats tl
      putStrLn "\nDiffList stats:"
      outputStats dd
      putStrLn ""
      printContextStats xs (tl ++ fst t, dd ++ snd t)
      
outputStats :: [Int] -> IO ()
outputStats l = 
  do      
  (to, mi, ma, lq, uq, me, mn) <- return (stats l)
  putStrLn $ "Min: " ++ (show mi) ++ " Max: " ++ (show ma)
  putStrLn $ "25%: " ++ (show lq) ++ " Median: " ++ (show me) ++ " 75%: " ++ (show uq)
  putStrLn $ "Total: " ++ (show to) ++ " Mean: " ++ (show mn)      

stats :: [Int] -> (Int, Int, Int, Int, Int, Int, Int)
stats ls = (total, minimum ls, maximum ls , lowquart, upquart, median, mean)
          where
            total = length ls
            lowquart = ls !! round (fromIntegral total * 0.25)
            upquart = ls !! round (fromIntegral total * 0.75)
            median = ls !! round (fromIntegral total * 0.5)
            mean = round ((fromIntegral (sum ls)) / (fromIntegral total))

usage :: [String] -> IO a
usage errs = if null errs then do
             hPutStrLn stdout use
             exitWith ExitSuccess
             else do
             hPutStrLn stderr (concat errs ++ "\n" ++ use)
             exitWith (ExitFailure (-1))
  where
  header = "Info - Output some information about an index.\n\n" ++
           "Usage: Info [OPTIONS]"
  use    = usageInfo header options

commandLineOpts :: [String] -> IO [Flag]
commandLineOpts argv = case getOpt Permute options argv of
                       (o, [], []  ) -> return o
                       (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option ['i'] ["index"] (ReqArg Index "FILE") "Loads index from FILE"
          , Option ['V'] ["version"] (NoArg Version) "Output version and exit"
          , Option ['?'] ["help"] (NoArg Help) "Output this help and exit"
          ]
