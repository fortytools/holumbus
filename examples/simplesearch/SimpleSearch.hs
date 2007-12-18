-- ----------------------------------------------------------------------------

{- |
  Module     : SimpleSearch
  Copyright  : Copyright (C) 2007 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  A simple example of Holumbus, providing a command line search with the
  default query language.

-}

-- ----------------------------------------------------------------------------

module Main where

import System.IO
import System.Environment
import System.Exit
import System.Console.Readline

import Char
import Data.Maybe

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Text.XML.HXT.Arrow
import Text.XML.HXT.DOM.Unicode

import Holumbus.Index.DocIndex
import Holumbus.Index.Common
import Holumbus.Index.Inverted
import Holumbus.Index.Convert
import Holumbus.Query.Parser
import Holumbus.Query.Processor
import Holumbus.Query.Result
import Holumbus.Data.StrMap as SM

main :: IO ()
main = do
       argv <- getArgs
       indexFile <- commandLineOpts argv
       putStrLn "Loading index ..."
       [invIndex] <- runX (loadIndex indexFile)
       putStr ("Loaded " ++ (show (IM.size (idToDoc (docTable invIndex)))) ++ " documents ")
       putStrLn ("containing " ++ show (M.fold (\p r -> (SM.size p) + r) 0 (indexParts invIndex)) ++ " words")
       answerQueries invIndex
       return ()

loadIndex :: String -> IOSArrow b InvIndex
loadIndex f = readDocument [(a_validate, v_0)] f
              >>>
              docIndexFromXml
              >>>
              convertIndex     
       
commandLineOpts :: [String] -> IO (String)
commandLineOpts [i] = do
                      return i
commandLineOpts _ = error "Usage: Holumbus INDEXFILE"

answerQueries :: InvIndex -> IO ()
answerQueries i = do
                  q <- readline ("Enter query (type :? for help) > ")
                  if isNothing q then answerQueries i
                    else
                      let n = fst $ utf8ToUnicode (fromJust q) in
                        do
                        addHistory n
                        answerQueries' n
  where
    answerQueries' :: String -> IO ()
    answerQueries' ""       = answerQueries i
    answerQueries' (':':xs) = internalCommand i xs
    answerQueries' q        = do
                              pr <- return (parseQuery q)
--                              putStrLn "Query:"
--                              putStrLn (show pq)
                              if null pr then do
                                putStrLn ("Could not parse query: " ++ q)
                                else do
                                  [(pq, e)] <- return pr
                                  if e == "" then do
                                    r <- return (process pq i (map fst (M.toList (indexParts i))))
                                    printDocHits (docHits r) i
                                    putStrLn ""
                                    printWordHits (wordHits r) i
                                    else do
                                      putStrLn ("Could not parse query: " ++ e)
                              answerQueries i

internalCommand :: InvIndex -> String -> IO ()
internalCommand _ "q"       = exitWith ExitSuccess
internalCommand i "?"       = do
                              putStrLn ""
                              printHelp
                              putStrLn ""
                              printContexts i
                              putStrLn ""
                              answerQueries i
--internalCommand i ('c':xs)  = do 
--                              if elem nc (map fst (M.toList (indexParts i))) then do
--                                answerQueries i
--                                else do
--                                  putStrLn "Unknown context!"
--                                  answerQueries i
--                               where
--                                 nc = dropWhile isSpace xs
internalCommand i _         = do
                              putStrLn "Unknown command!"
                              answerQueries i

printDocHits :: DocHits -> InvIndex -> IO ()
printDocHits h i = do
                   putStrLn "Result:"
                   printHits' (map fromJust (map (\d -> IM.lookup d (idToDoc (docTable i))) 
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


printWordHits :: WordHits -> InvIndex -> IO ()
printWordHits h _ = do
                    putStrLn "Completions:"
                    d <- return (L.sortBy (compare `on` snd) (map (\(c, o) -> (c, M.fold (\m r -> r + IM.size m) 0 o)) (M.toList h)))
                    putStrLn (foldr (\(c, s) r -> r ++ c ++ " (" ++ (show s) ++ ") ") "" d)
                    putStrLn ""
                    putStrLn ("Found " ++ (show (M.size h)) ++ " possible completions")

printHelp :: IO ()
printHelp = do
            putStrLn "Holumbus treats single words as prefix terms and will give you possible completions."
            putStrLn "Words are interpreted case insensitive. Phrases and exact matches (case sensitive)"
            putStrLn "can be specified by using quotes (i.e. \"Foo Bar\" will match this exact sequence)."
            putStrLn "Terms just separated by space will be treated implicitly as AND terms."
            putStrLn "Other operators have to be specified explisitly. Avaliable operators are: AND, OR, NOT"
            putStrLn "Priority can be influenced by round parantheses."
            putStrLn "The contexts to search can be restricted with the : operator (seperate them with , )."
            putStrLn "Example: firstcontext,secondcontext:(foo OR bar) NOT foobar"
            putStrLn "This will search for documents containing \"foo\" or \"bar\" in the contexts named"
            putStrLn "\"firstcontext\" and \"secondcontext\" and no \"foobar\" in the all contexts."
            putStrLn ""
            putStrLn "Use :q to exit and :? to show this help."
            return ()

printContexts :: InvIndex -> IO ()
printContexts i = do
                  putStrLn "Avaliable contexts:"
                  printContexts' (map fst (M.toList (indexParts i)))
                  where
                    printContexts' :: [String] -> IO ()
                    printContexts' [] = return ()
                    printContexts' (x:xs) = do
                                            putStrLn x
                                            printContexts' xs
                                            return ()

convertIndex :: IOSArrow DocIndex InvIndex
convertIndex = arr hyphoonToInvHolumbus

-- This is a fix for GHC 6.6.1 (from 6.8.1 on, this is avaliable in module Data.Function)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(*) `on` f = \x y -> f x * f y