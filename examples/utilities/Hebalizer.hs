-- ----------------------------------------------------------------------------

{- |
  Module     : Convert
  Copyright  : Copyright (C) 2008 Sebastian M. Schlatt
  License    : MIT

  Maintainer : Sebastian M. Schlatt
  Stability  : experimental
  Portability: untested
  Version    : 0.1

  Create HTML Statistics from Hayoo log files

-}

-- ----------------------------------------------------------------------------

module Main where

import Holumbus.Utility
import Holumbus.Control.MapReduce.Parallel
import Text.XML.HXT.Arrow
import Numeric

import Data.Map hiding (split, map, filter)

main :: IO ()
main 
  = do
    logs    <- readFile ("/srv/www/holumbus.schlatt.com/janus/build/hayoo.log")
    entries <- return $! filter (/= "") (split "\n" logs)
    stats   <- mapReduce 1 processLog makeStatistics (zip (repeat 42) entries)
    withMax <- return $! getMaximums (toList stats)
    runX (mkHtml withMax >>> writeDocument [] "/srv/www/holumbus.schlatt.com/htdocs/stats.html")
    return ()
       
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
          , selem "td" [ mkelem "img" [ sattr "src" "http://www.holumbus.com/blau.gif"
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
   

