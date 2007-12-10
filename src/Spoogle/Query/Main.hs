-- ----------------------------------------------------------------------------

{- |
  Module     : Spoogle.Main
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Hübel
  License    : MIT

  Maintainer : Timo B. Hübel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : $Id$

  The inverted index for Spoogle.

-}

-- ----------------------------------------------------------------------------

module Main where

import System.IO
import System.Environment
import System.Exit
import System.Console.Readline

import Char
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM

import Text.XML.HXT.Arrow

import Hyphoon.DocIndex

import Spoogle.Index.Inverted
import Spoogle.Index.Convert
import Spoogle.Query.Parser
import Spoogle.Query.Processor

main :: IO ()
main = do
       argv <- getArgs
       (indexFile, defaultContext) <- commandLineOpts argv
       putStrLn "Loading index ..."
       [invIndex] <- runX (loadIndex indexFile)
       putStrLn ((show (IM.size (idToDoc (docTable invIndex)))) ++ " documents loaded")
       answerQueries invIndex defaultContext
       return ()

loadIndex :: String -> IOSArrow b InvIndex
loadIndex f = readDocument [(a_validate, v_0)] f
              >>>
              docIndexFromXml
              >>>
              convertIndex     
       
commandLineOpts :: [String] -> IO ((String, String))
commandLineOpts [i, c] = do
                         return (i, c)
commandLineOpts _ = error "Usage: Spoogle INDEXFILE DEFAULTCONTEXT"

answerQueries :: InvIndex -> String -> IO ()
answerQueries i c = do
                    q <- readline ("Enter query (type :? for help) " ++ c ++ "> ")
                    if isNothing q then answerQueries i c
                      else
                        do
                        addHistory (fromJust q)
                        answerQueries' (fromJust q)
  where
    answerQueries' :: String -> IO ()
    answerQueries' ""       = answerQueries i c
    answerQueries' (':':xs) = internalCommand i c xs
    answerQueries' q        = do
                              [(pq, e)] <- return (parse query q)
--                              putStrLn "Query:"
--                              putStrLn (show pq)
                              if e == "" then do
                                r <- return (process pq i c)
                                printHits (hits r) i
                                putStrLn ""
                                printHints (hints r)
                                else do
                                  putStrLn ("Could not parse query: " ++ e)
                              answerQueries i c

internalCommand :: InvIndex -> String -> String -> IO ()
internalCommand _ _ "q"       = exitWith ExitSuccess
internalCommand i c "?"       = do
                                putStrLn ""
                                printHelp
                                putStrLn ""
                                printContexts i
                                putStrLn ""
                                answerQueries i c
internalCommand i c ('c':xs)  = do 
                                if elem nc (map fst (M.toList (indexParts i))) then do
                                  answerQueries i nc
                                  else do
                                    putStrLn "Unknown context!"
                                    answerQueries i c
                                 where
                                   nc = dropWhile isSpace xs
internalCommand i c _         = do
                                putStrLn "Unknown command!"
                                answerQueries i c

printHits :: Hits -> InvIndex -> IO ()
printHits h i = do
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


printHints :: Hints -> IO ()
printHints h = do
               putStrLn "Completions:"
               putStrLn (foldl (\r c -> r ++ c ++ " ") "" (S.toList h))
               putStrLn ""
               putStrLn ("Found " ++ (show (S.size h)) ++ " possible completions")

printHelp :: IO ()
printHelp = do
            putStrLn "Spoogle treats single words as prefix terms and will give you possible completions."
            putStrLn "Terms just separated by space will be treated implicitly as AND terms."
            putStrLn "Other operators have to be specified explisitly. Avaliable operators are: AND, OR, NOT"
            putStrLn "Priority can be influenced by round parantheses."
            putStrLn "A context other than the default context can be specified with the : operator."
            putStrLn "Example: context:(foo OR bar) NOT foobar"
            putStrLn "This will search for documents containing \"foo\" or \"bar\" in the context named"
            putStrLn "\"context\" and no \"foobar\" in the default context."
            putStrLn ""
            putStrLn "Use :c context to set a new default context, :q to exit and :? to show this help."
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
convertIndex = arr hyphoonToInvSpoogle