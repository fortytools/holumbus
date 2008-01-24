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

import Text.XML.HXT.DOM.Unicode

import qualified Holumbus.Index.Inverted as INV
import qualified Holumbus.Index.Hybrid as HYB
import Holumbus.Index.Common hiding (contexts)

data Flag = Inv String | Hyb String | Con Context | Version deriving (Show, Eq)

version :: String
version = "0.1"

main :: IO ()
main = do
       argv <- getArgs
       flags <- commandLineOpts argv
       if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
       contexts <- return (filter isContext flags)
       if null contexts then usage ["No context given!\n"] else return ()
       if length contexts > 1 then usage ["Only one context allowed!\n"] else return ()
       indexes <- return (filter isIndex flags)
       if null indexes then usage ["No index file given!\n"] else return ()
       if length indexes > 1 then usage ["Only one index file allowed!\n"] else return ()
       context <- fromContext $ head contexts
       startup context (head indexes)
       return ()

isContext :: Flag -> Bool
isContext (Con _) = True
isContext _ = False

isIndex :: Flag -> Bool
isIndex (Inv _) = True
isIndex (Hyb _) = True
isIndex _ = False

fromContext :: Flag -> IO (Context)
fromContext (Con s) = return s
fromContext _ = do
                usage ["Internal error!\n"]

-- | Decide between hybrid and inverted and then fire up!
startup :: Context -> Flag -> IO ()
startup c (Inv file) = do
                       idx <- INV.loadFromXmlFile file
                       return (rnf idx)
                       printWords c idx
startup c (Hyb file) = do
                       idx <- HYB.loadFromXmlFile file
                       printWords c idx
startup _ _ = do
              usage ["Internal error!\n"]

printWords :: HolIndex i => Context -> i -> IO ()
printWords c idx = do
                   w <- return (concat $ L.sort $ map (\(w, _) -> unicodeToUtf8 w ++ "\n") (allWords c idx))
                   putStrLn w
                   exitWith ExitSuccess

usage :: [String] -> IO a
usage errs = if null errs then do
             hPutStrLn stdout use
             exitWith ExitSuccess
             else do
             hPutStrLn stderr (concat errs ++ "\n" ++ use)
             exitWith (ExitFailure (-1))
  where
  header = "Words - Output all unique words of an index.\n\n" ++
           "Usage: Words [OPTIONS]"
  use    = usageInfo header options

commandLineOpts :: [String] -> IO [Flag]
commandLineOpts argv = case getOpt Permute options argv of
                       (o, [], []  ) -> return o
                       (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option ['i'] ["inverted"] (ReqArg Inv "FILE") "Loads inverted index from FILE"
          , Option ['h'] ["hybrid"]   (ReqArg Hyb "FILE") "Loads hybrid index from FILE"
          , Option ['c'] ["context"]  (ReqArg Con "CONTEXT") "Show words from context CONTEXT"
          , Option ['V'] ["version"]  (NoArg Version)     "Output version and exit"
          ]
