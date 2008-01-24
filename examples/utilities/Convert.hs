-- ----------------------------------------------------------------------------

{- |
  Module     : Convert
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (t.h@gmx.info)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Convert indexes between XML and binary format.

-}

-- ----------------------------------------------------------------------------

module Main where

import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt

import qualified Data.List as L

import qualified Holumbus.Index.Inverted as INV

data Flag = InInvBin String 
          | InInvXml String 
          | OutInvBin String
          | OutInvXml String
          | Version deriving (Show, Eq)

version :: String
version = "0.1"

main :: IO ()
main = do
       argv <- getArgs
       flags <- commandLineOpts argv
       if Version `elem` flags then (putStrLn version) >> (exitWith ExitSuccess) else return ()
       input <- return (filter isInput flags)
       if null input then usage ["No input file given!\n"] else return ()
       if length input > 1 then usage ["Only one input file allowed!\n"] else return ()
       output <- return (filter isOutput flags)
       if null output then usage ["No output file given!\n"] else return ()
       if length output > 1 then usage ["Only one output file allowed!\n"] else return ()
       startup (head input) (head output)
       return ()

isInput :: Flag -> Bool
isInput (InInvBin _) = True
isInput (InInvXml _) = True
isInput _ = False

isOutput :: Flag -> Bool
isOutput (OutInvBin _) = True
isOutput (OutInvXml _) = True
isOutput _ = False

-- | Decide between hybrid and inverted and then fire up!
startup :: Flag -> Flag -> IO ()
startup (InInvXml ifp) (OutInvBin ofp) = do
                                         idx <- INV.loadFromXmlFile ifp
                                         INV.writeToBinFile ofp idx
                                         exitWith ExitSuccess
startup (InInvBin ifp) (OutInvXml ofp) = do
                                         idx <- INV.loadFromBinFile ifp
                                         INV.writeToXmlFile ofp idx
                                         exitWith ExitSuccess
startup _ _ = do
              usage ["File already has the correct format!\n"]

usage :: [String] -> IO a
usage errs = if null errs then do
             hPutStrLn stdout use
             exitWith ExitSuccess
             else do
             hPutStrLn stderr (concat errs ++ "\n" ++ use)
             exitWith (ExitFailure (-1))
  where
  header = "Convert - Convert indexes between various types and formats.\n\n" ++
           "Usage: Convert [OPTIONS]"
  use    = usageInfo header options

commandLineOpts :: [String] -> IO [Flag]
commandLineOpts argv = case getOpt Permute options argv of
                       (o, [], []  ) -> return o
                       (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option ['b'] ["input-inverted-binary"]  (ReqArg InInvBin "FILE") "Loads inverted index from binary FILE"
          , Option ['x'] ["input-inverted-xml"]     (ReqArg InInvXml "FILE") "Loads inverted index from XML FILE"
          , Option ['i'] ["output-inverted-binary"] (ReqArg OutInvBin "FILE") "Outputs the loaded index as inverted into binary FILE"
          , Option ['m'] ["output-inverted-xml"]    (ReqArg OutInvXml "FILE") "Outputs the loaded index as inverted into XML FILE"
          , Option ['V'] ["version"]  (NoArg Version)     "Output version and exit"
          ]
