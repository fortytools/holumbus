-- ----------------------------------------------------------------------------

{- |
  Module     : Hebalizer
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT

  Maintainer : Sebastian M. Schlatt
  Stability  : experimental
  Portability: untested
  Version    : 0.1

  Create HTML Statistics from Hayoo log files.

-}

-- ----------------------------------------------------------------------------

module Main where

import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt

import Text.XML.HXT.Arrow

import Holumbus.Utility
import Holumbus.Control.MapReduce.Parallel

import qualified Data.List as L
import Data.Map hiding (split, map, filter, null)

data Flag = Input String 
          | Output String 
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

  inp <- return (filter isInput flags)
  if L.null inp then usage ["No input file given!\n"] else return ()
  if length inp > 1 then usage ["Only one input file allowed!\n"] else return ()

  out <- return (filter isOutput flags)
  if L.null out then usage ["No output file given!\n"] else return ()
  if length out > 1 then usage ["Only one output file allowed!\n"] else return ()
 
  startup (head inp) (head out)
  return ()
       
startup :: Flag -> Flag -> IO ()
startup (Input inp) (Output out) = do  
  logs    <- readFile inp
  entries <- return $! filter (/= "") (split "\n" logs)
  stats   <- mapReduce 1 processLog makeStatistics (zip (repeat 42) entries)
  withMax <- return $! getMaximums (toList stats)
  runX (mkHtml withMax >>> writeDocument [(a_indent, "1")] out)
  return ()

startup _ _ = usage ["Internal error!\n"]

isInput :: Flag -> Bool
isInput (Input _) = True
isInput _ = False
       
isOutput :: Flag -> Bool
isOutput (Output _) = True
isOutput _ = False

getMaximums :: [(String, [(String, Int)])] -> [(String, Int, [(String, Int)])]
getMaximums = map addMaximum
  where
  addMaximum (sectionName, values) = (sectionName, getMaximum values, values)
  getMaximum values = maximum (map snd values)
       
mkHtml :: ArrowXml a => [(String, Int, [(String, Int)])] -> a b XmlTree
mkHtml l = 
     root [] [
          selem "html" 
              [ selem "head" 
                [ selem "title"
                  [ constA "Hayoo! Usage Statistics" >>> mkText
                  ]
                ]
              , selem "body"
                ([ selem "h1" [ constA "Hayoo! Usage Statistics" >>> mkText]
                ] ++ ( map mkSection l))
              ]
              ]
    >>> addXHtmlDoctypeTransitional                      
            
              
    
mkSection :: ArrowXml a => (String, Int, [(String, Int)]) -> a b XmlTree
mkSection (sectionName, maxVal, values) =
    mkelem "div" [sattr "id" sectionName] 
      [ selem "h2" [ constA sectionName >>> mkText] 
      , selem "table"
        [ selem "tbody"
          (map (mkTr maxVal) values)         
        ]
      ] 
    where
      mkTr maxVal (value, count) =
        selem "tr"
          [ selem "td" [ constA value        >>> mkText]
          , selem "td" [ constA (show count) >>> mkText]
          , selem "td" [ mkelem "img" [ sattr "src" "http://www.holumbus.org/blau.gif"
                                      , sattr "height" "12"
                                      , sattr "width" (show $ round ((fromIntegral count) * (fromIntegral maxImageWidth) / (fromIntegral maxVal)))
                                      ]
                                      [] 
                       ]
          ]
      maxImageWidth = 300

processLog :: Int -> String -> IO [(String, String)]             -- the MAP
processLog _ entry 
  = do
    s <- return $! split "\t" entry
    return $ [ ("ip"   , s !! 1)
             , ("query", s !! 3) 
             ]

makeStatistics :: String -> [String] -> IO (Maybe [(String, Int)])        -- the REDUCE
makeStatistics _ values 
  = return $ Just $ toList $
    foldl theFunc empty values
    where
      theFunc :: Map String Int -> String -> Map String Int
      theFunc m v =
         insertWith (+) v 1 m   
   
usage :: [String] -> IO a
usage errs = if null errs then do
             hPutStrLn stdout use
             exitWith ExitSuccess
             else do
             hPutStrLn stderr (concat errs ++ "\n" ++ use)
             exitWith (ExitFailure (-1))
  where
  header = "Hebalizer - Generate statistics from Hayoo! log files.\n\n" ++
           "Usage: Hebalizer [OPTIONS]"
  use    = usageInfo header options

commandLineOpts :: [String] -> IO [Flag]
commandLineOpts argv = case getOpt Permute options argv of
                       (o, [], []  ) -> return o
                       (_, _, errs) -> usage errs

options :: [OptDescr Flag]
options = [ Option ['i'] ["input"] (ReqArg Input "FILE") "Loads log data from FILE"
          , Option ['o'] ["output"] (ReqArg Output "FILE") "Outputs statistics to FILE"
          , Option ['V'] ["version"] (NoArg Version) "Output version and exit"
          , Option ['?'] ["help"] (NoArg Help) "Output this help and exit"
          ]
