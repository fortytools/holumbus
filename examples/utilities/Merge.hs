-- ----------------------------------------------------------------------------

{- |
  Module     : Merge
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Merges two indexes and their document tables.

-}

-- ----------------------------------------------------------------------------

module Main where

import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt

import Holumbus.Index.Inverted (InvIndex)
import Holumbus.Index.Documents (Documents)
import Holumbus.Index.Common

data Flag = Index String 
          | Documents String 
          | Version 
          | Help deriving (Show, Eq)

version :: String
version = "0.3"

main :: IO ()
main = do
       argv <- getArgs
       flags <- commandLineOpts argv

       if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
       if Help `elem` flags then usage [] >> (exitWith ExitSuccess) else return ()

       idx <- return (filter isIndex flags)
       if null idx then usage ["No index files given!\n"] else return ()
       if length idx /= 2 then usage ["Only two index files allowed!\n"] else return ()

       doc <- return (filter isDocuments flags)
       if null doc then usage ["No documents files given!\n"] else return ()
       if length doc /= 2 then usage ["Only two documents files allowed!\n"] else return ()

       if length doc /= length idx then usage ["Exactly two indexes with their documents required!\n"] else return ()

       startup idx doc
       return ()

isIndex :: Flag -> Bool
isIndex (Index _) = True
isIndex _ = False

isDocuments :: Flag -> Bool
isDocuments (Documents _) = True
isDocuments _ = False

-- | Load indexes and merge.
startup ::[Flag] -> [Flag] -> IO ()
startup [(Index if1), (Index if2)] [(Documents df1), (Documents df2)] = 
  do
  putStrLn "Loading indexes..."
  idx1 <- (loadFromFile if1) :: IO InvIndex
  idx2 <- (loadFromFile if2) :: IO InvIndex
  putStrLn "Loading documents..."
  doc1 <- (loadFromFile df1) :: IO (Documents Int)
  doc2 <- (loadFromFile df2) :: IO (Documents Int)
  putStrLn "Merging..."
  (d, i) <- return (mergeAll doc1 idx1 doc2 idx2)
  writeToBinFile "merged-docs.bin" d
  writeToBinFile "merged-index.bin" i
  putStrLn "Finished!"
startup _ _ = usage ["Internal error!\n"]

usage :: [String] -> IO a
usage errs = if null errs then do
             hPutStrLn stdout use
             exitWith ExitSuccess
             else do
             hPutStrLn stderr (concat errs ++ "\n" ++ use)
             exitWith (ExitFailure (-1))
  where
  header = "Merge - Merges two indexes and their document tables.\n\n" ++
           "Usage: Merge [OPTIONS]"
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
