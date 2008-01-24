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
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import qualified Holumbus.Index.Inverted as INV
import qualified Holumbus.Index.Hybrid as HYB
import Holumbus.Index.Common

data Flag = Inv String | Hyb String | Version deriving (Show, Eq)

version :: String
version = "0.1"

main :: IO ()
main = do
       argv <- getArgs
       flags <- commandLineOpts argv
       if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
       indexes <- return (filter isIndex flags)
       if null indexes then usage ["No index file given!\n"] else return ()
       if length indexes > 1 then usage ["Only one index file allowed!\n"] else return ()
       startup (head indexes)
       return ()

isIndex :: Flag -> Bool
isIndex (Inv _) = True
isIndex (Hyb _) = True
isIndex _ = False

-- | Decide between hybrid and inverted and then fire up!
startup :: Flag -> IO ()
startup (Inv file) = do
                       idx <- INV.loadFromXmlFile file
                       return (rnf idx)
                       printStats idx
startup (Hyb file) = do
                       idx <- HYB.loadFromXmlFile file
                       printStats idx
startup _ = do
              usage ["Internal error!\n"]

printStats :: HolIndex i => i -> IO ()
printStats i = do
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
                noUniqueDocs = sizeDocs i
                noUniqueWords = sizeWords i
                noDocs = foldr (\c r -> foldr (\(_, o) n -> n + IM.size o) r (allWords c i)) 0 (contexts i)
                noWords = foldr (\c r -> foldr (\(_, o) n -> IM.fold ((+) . IS.size) n o) r (allWords c i)) 0 (contexts i)

printContextStats :: HolIndex i => [Context] -> i -> IO ()
printContextStats [] _ = return ()
printContextStats (c:cs) i = do 
                             putStrLn ""
                             putStrLn $ "Context: " ++ c
                             putStrLn $ "Unique words: " ++ (show noUniqueWords)
                             putStrLn $ "Words: " ++ (show noWords)
                             printContextStats cs i
                             where
                               noUniqueWords = length $ allWords c i
                               noWords = foldr (\(_, o) r -> IM.fold ((+) . IS.size) r o) 0 (allWords c i)

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
options = [ Option ['i'] ["inverted"] (ReqArg Inv "FILE") "Loads inverted index from FILE"
          , Option ['h'] ["hybrid"]   (ReqArg Hyb "FILE") "Loads hybrid index from FILE"
          , Option ['V'] ["version"]  (NoArg Version)     "Output version and exit"
          ]
