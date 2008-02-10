-- ----------------------------------------------------------------------------

{- |
  Module     : Split
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Split an index into a number of (possible smaller) indexes.

-}

-- ----------------------------------------------------------------------------

module Main where

import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt

import Data.Maybe

import Control.Parallel.Strategies

import qualified Data.List as L

import Holumbus.Index.Inverted
import Holumbus.Index.Common

data Flag = Index String 
          | Split String
          | Number String
          | Version 
          | Help deriving (Show, Eq)

data Split = Documents
           | Words
           | Contexts

version :: String
version = "0.1"

main :: IO ()
main = do
       argv <- getArgs
       flags <- commandLineOpts argv
       if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
       if Help `elem` flags then usage [] >> (exitWith ExitSuccess) else return ()

       input <- return (filter isInput flags)
       if null input then usage ["No input file given!\n"] else return ()
       if length input > 1 then usage ["Only one input file allowed!\n"] else return ()

       split <- return (filter isSplit flags)
       if null split then usage ["No splitting option given!\n"] else return ()
       if length split > 1 then usage ["Only one splitting option allowed!\n"] else return ()

       splt <- return $ getSplit (head split)
       if isNothing splt then usage ["Unknown format!\n"] else return ()

       num <-return (filter isNumber flags)
       if null num then usage ["No amount of splits given!\n"] else return ()
       if length num > 1 then usage ["Only one amount of splits allowed!\n"] else return ()

       startup (head input) (fromJust splt) (getNumber $ head num)
       return ()

getNumber :: Flag -> Int
getNumber (Number n) = read n
getNumber _ = error "Internal error!"

getSplit :: Flag -> Maybe Split
getSplit (Split "documents") = Just Documents
getSplit (Split "words") = Just Words
getSplit (Split "contexts") = Just Contexts
getSplit _ = Nothing

isInput :: Flag -> Bool
isInput (Index _) = True
isInput _ = False

isSplit :: Flag -> Bool
isSplit (Split _) = True
isSplit _ = False

isNumber :: Flag -> Bool
isNumber (Number _) = True
isNumber _ = False

-- | Decide between split options and fire up!
startup :: Flag -> Split -> Int -> IO ()
startup (Index inp) Words n = do
                              putStrLn ("Splitting " ++ inp ++ " into " ++ (show n) ++ " parts by words...")
                              idx <- (loadFromFile inp) :: IO InvIndex
                              return (rnf idx)
                              writeIndexes (splitByWords idx n) inp
startup (Index inp) Documents n = do
                                  putStrLn ("Splitting " ++ inp ++ " into " ++ (show n) ++ " parts by documents...")
                                  idx <- (loadFromFile inp) :: IO InvIndex
                                  return (rnf idx)
                                  writeIndexes (splitByDocuments idx n) inp
startup (Index inp) Contexts n = do
                                 putStrLn ("Splitting " ++ inp ++ " into " ++ (show n) ++ " parts by contexts...")
                                 idx <- (loadFromFile inp) :: IO InvIndex
                                 return (rnf idx)
                                 writeIndexes (splitByContexts idx n) inp
startup _ _ _ = usage ["Internal error!\n"]

writeIndexes :: [InvIndex] -> FilePath -> IO ()
writeIndexes [] _ = return ()
writeIndexes (i:is) f = do
                        writeToBinFile (f ++ "." ++ (show $ length is)) i
                        writeIndexes is f

usage :: [String] -> IO a
usage errs = if null errs then do
             hPutStrLn stdout use
             exitWith ExitSuccess
             else do
             hPutStrLn stderr (concat errs ++ "\n" ++ use)
             exitWith (ExitFailure (-1))
  where
  header = "Split - Split an index into a number of (smaller) indexes.\n\n" ++
           "Usage: Split [OPTIONS] where SPLIT is one of the following:\n\n" ++
           "documents - Splitting by documents\n" ++
           "words - Splitting by words\n" ++
           "contexts - Splitting by contexts\n\n" ++
           "Avaliable options:" 
  use    = usageInfo header options

commandLineOpts :: [String] -> IO [Flag]
commandLineOpts argv = case getOpt Permute options argv of
                       (o, [], []  ) -> return o
                       (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option "i" ["index"] (ReqArg Index "FILE") "Loads index from FILE"
          , Option "s" ["split"] (ReqArg Split "SPLIT") "Split the index by SPLIT"
          , Option "n" ["number"] (ReqArg Number "NUM") "Split into NUM indexes"
          , Option ['V'] ["version"]  (NoArg Version)     "Output version and exit"
          , Option ['?'] ["help"]  (NoArg Help)     "Output this help and exit"
          ]
