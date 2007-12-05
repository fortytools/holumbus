-- ----------------------------------------------------------------------------

{- |
   Module     : Spoogle.Main
   Copyright  : Copyright (C) 2007 Sebastian M. Schlatt, Timo B. Hübel
   License    : MIT

   Maintainer : Timo B. Hübel
   Maintainer : t.h@gmx.info
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
       putStrLn "Loading index..."
       [invIndex] <- runX (loadIndex indexFile)
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
                    putStrLn "Enter query:"
                    q <- getLine
                    answerQueries' q
  where
    answerQueries' :: String -> IO ()
    answerQueries' "" = do
                        putStrLn "Enter query:"
                        nq <- getLine
                        answerQueries' nq
    answerQueries' ":q" = exitWith ExitSuccess
    answerQueries' q = do
                       [(pq, e)] <- return (parse query q)
--                       putStrLn "Query:"
--                       putStrLn (show pq)
                       if e == "" then do
                         putStrLn "Result:"
                         r <- return (process pq i c)
                         putStrLn (show r)
                         (d, w) <- return (transform r i)
                         putStrLn ("Found " ++ (show (length d)) ++ " documents")
                         putStrLn (show d)
                         putStrLn "Completions:"
                         putStrLn (show w)
                         else do
                           putStrLn ("Could not parse query: " ++ e)
                       putStrLn "Enter query:"
                       nq <- getLine
                       answerQueries' nq

convertIndex :: IOSArrow DocIndex InvIndex
convertIndex = arr hyphoonToInvSpoogle