-- ----------------------------------------------------------------------------

{- |
  Module     : SimpleSearch
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.4

  A simple example of Holumbus, providing a command line search with the
  default query language.

-}

-- ----------------------------------------------------------------------------

module Main where

import System.IO
import System.Environment
import System.Exit
import System.Console.Readline
import System.Console.GetOpt
import System.CPUTime

import Char
import Data.Maybe

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Text.XML.HXT.DOM.Unicode

import qualified Holumbus.Index.Inverted as INV
import qualified Holumbus.Index.Hybrid as HYB
import Holumbus.Index.Common
import Holumbus.Index.Combined
import Holumbus.Index.Documents
import Holumbus.Query.Syntax
import Holumbus.Query.Parser
import Holumbus.Query.Processor
import Holumbus.Query.Ranking
import Holumbus.Query.Result hiding (sizeDocs, sizeWords)

data Flag = Inverted String | Hybrid String | Verbose | Version deriving (Show, Eq)

version :: String
version = "0.4"

main :: IO ()
main = do
       argv <- getArgs
       flags <- commandLineOpts argv
       if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
       verbose <- return (Verbose `elem` flags)
       indexes <- return (filter (\f -> f /= Verbose && f /= Version) flags)
       if L.null indexes then usage ["No index file given!\n"] else return ()
       if length indexes > 1 then usage ["Only one index file allowed!\n"] else return ()
       putStrLn "Loading index ..."
       startup verbose (head indexes)
       return ()

-- | Decide between hybrid and inverted and then fire up!
startup :: Bool -> Flag -> IO ()
startup v (Inverted file) = do
                            inv <- INV.loadFromFile file
                            idx <- return (Inv inv)
                            printStats idx
                            answerQueries v idx
startup v (Hybrid file) = do
                          hyb <- HYB.loadFromFile file
                          idx <- return (Hyb hyb)
                          printStats idx
                          answerQueries v idx
startup _ _ = do
              usage ["Internal error!\n"]
                          
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
options = [ Option ['i'] ["inverted"] (ReqArg Inverted "FILE") "Loads inverted index from FILE"
          , Option ['h'] ["hybrid"]   (ReqArg Hybrid "FILE") "Loads hybrid index from FILE"
          , Option ['v'] ["verbose"]  (NoArg Verbose)          "Be more verbose"
          , Option ['V'] ["version"]  (NoArg Version)          "Output version and exit"
          ]

answerQueries :: Bool -> AnyIndex -> IO ()
answerQueries verbose i = do
                          q <- readline ("Enter query (type :? for help) > ")
                          if isNothing q then answerQueries verbose i else
                            do
                            n <- return (fst $ utf8ToUnicode (fromJust q))
                            addHistory n
                            answerQueries' n
  where
  answerQueries' :: String -> IO ()
  answerQueries' ""       = answerQueries verbose i
  answerQueries' (':':xs) = internalCommand verbose i xs
  answerQueries' q        = do
                            pr <- return (parseQuery q)
                            if verbose then putStrLn ("Query: \n" ++ (show pr) ++ "\n") else return ()
                            either printError makeQuery pr
                            answerQueries verbose i
    where
    printError err = putStrLn ("Problem parsing query: " ++ err)
    makeQuery pq = do
                   t1 <- getCPUTime
                   oq <- return (optimize pq)
                   if verbose then putStrLn ("Optimized: \n" ++ (show oq) ++ "\n") else return ()
                   r <- return (process oq i (contexts i))
                   rr <- return (rank r)
                   printDocHits (docHits rr) (documents i)
                   putStrLn ""
                   printWordHits (wordHits rr)
                   t2 <- getCPUTime
                   s <- return (show $ round $ (fromIntegral $ (t2 - t1)) / 1000000000000)
                   m <- return (show $ round $ (fromIntegral $ (t2 - t1)) / 1000000000)
                   putStrLn ""
                   putStrLn ("Query processed in " ++ s ++ "." ++ m ++ " sec")

internalCommand :: Bool -> AnyIndex -> String -> IO ()
internalCommand _       _ "q"       = exitWith ExitSuccess
internalCommand verbose i "?"       = do
                                      putStrLn ""
                                      printHelp
                                      putStrLn ""
                                      printContexts i
                                      putStrLn ""
                                      answerQueries verbose i
internalCommand verbose i _         = do
                                      putStrLn "Unknown command!"
                                      answerQueries verbose i

printDocHits :: DocHits -> Documents -> IO ()
printDocHits h docs = do
                      putStrLn "Result:"
                      printHits' (map fromJust (map (\d -> IM.lookup d (idToDoc docs)) 
                                 (IM.foldWithKey (\k _ l -> k:l) [] h)))
                      putStrLn ""
                      putStrLn ("Found " ++ (show (IM.size h)) ++ " documents")
                      where
                        printHits' :: [Document] -> IO ()
                        printHits' [] = return ()
                        printHits' ((t, u):xs) = do
                                                 putStrLn t
                                                 putStrLn u
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

printContexts :: AnyIndex -> IO ()
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

printStats :: AnyIndex -> IO ()
printStats i = do
               putStr ("Loaded " ++ (show (sizeDocs i)) ++ " documents ")
               putStrLn ("containing " ++ show (sizeWords i) ++ " words")

-- This is a fix for GHC 6.6.1 (from 6.8.1 on, this is avaliable in module Data.Function)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
op `on` f = \x y -> f x `op` f y
