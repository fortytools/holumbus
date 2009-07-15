-- ----------------------------------------------------------------------------
{- |
  Module     : Examples.MapReduce.Grep.Grep
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Examples.MapReduce.Grep.Grep
(
  grepAction
, grepActionMap
, namesList
, grepDemoJob
, createGrepDemoFiles
)
where

--import           Data.Binary
import           Holumbus.Common.MRBinary
import           Data.Maybe

import           System.Log.Logger

import           Text.Regex

import qualified Holumbus.FileSystem.FileSystem as FS

import qualified Holumbus.Data.KeyMap as KMap
import           Holumbus.MapReduce.Types


localLogger :: String
localLogger = "Examples.MapReduce.Grep.Grep"


-- ----------------------------------------------------------------------------
-- Distributed Grep
-- ----------------------------------------------------------------------------

-- grep for the namelist example
mapGrep :: ActionEnvironment -> String -> () -> String -> IO [((), String)]
mapGrep _ a _ v
  = do 
    infoM localLogger "mapGrep"
    debugM localLogger $ "input: a: " ++ show a ++ " - Text: " ++ show v
    res <- if (matches a v)
      then return [((),v)]
      else return []
    debugM localLogger $ "output: " ++ show res
    return res
    where
    matches e n = isJust $ matchRegex (mkRegex e) n

{-
-- grep for (Filename,Text)
mapGrep :: ActionEnvironment -> String -> String -> String -> IO [((),(String,String))]
mapGrep _ e k v
  = do
    return $ mapMaybe (getMatch) $ lines v
    where
    getMatch l 
      = if (isJust $ matchRegex (mkRegex e) l)
        then Just ((),(k,l))
        else Nothing    
-}  

-- reduce for the namelist example
reduceGrep :: ActionEnvironment -> String -> () -> [String] -> IO (Maybe [String])
reduceGrep _ _ _ vs 
  = do
    infoM localLogger "reduce/combine Grep"
    debugM localLogger $ "input: " ++ show vs
    return (Just vs)

{-
-- reduce for (Filename,Text)
reduceGrep
  :: ActionEnvironment -> String -> () -> [(String,String)] -> IO (Maybe [(String,String)])
reduceGrep _ _ _ vs 
  = do
    infoM localLogger "reduce/combine Grep"
    debugM localLogger $ "input: " ++ show vs
    return (Just vs)
-}

-- ----------------------------------------------------------------------------
-- Actions
-- ----------------------------------------------------------------------------

-- Action for the namelist example
grepAction
  :: ActionConfiguration 
       String                                      -- state
       () String                                   -- k1, v1
       () String                                   -- k2, v2
       String                                      -- v3 == v2
       [String]                                    -- v4
grepAction
  = (defaultActionConfiguration "GREP")
        { ac_Map     = Just mapAction
        , ac_Combine = Nothing
        , ac_Reduce  = Just reduceAction
        }
    where
      mapAction 
        = (defaultMapConfiguration mapGrep)
      reduceAction
        = (defaultReduceConfiguration reduceGrep)

{-
-- Action for (Filename,Text)
grepAction
  :: ActionConfiguration 
       String                                      -- state
       String String                               -- k1, v1
       () (String,String)                          -- k2, v2
       (String, String)                            -- v3 == v2
       [(String, String)]                          -- v4
grepAction
  = (defaultActionConfiguration "GREP")
        { ac_Map     = Just mapAction
        , ac_Combine = Nothing
        , ac_Reduce  = Just reduceAction
        }
    where
      mapAction 
        = (defaultMapConfiguration mapGrep)
      reduceAction
        = (defaultReduceConfiguration reduceGrep)
-}


grepActionMap :: ActionMap
grepActionMap
  = KMap.insert (readActionConfiguration grepAction) $
    KMap.empty


-- ----------------------------------------------------------------------------
-- DemoJob
-- ----------------------------------------------------------------------------

  
grepDemoJob :: JobInfo
grepDemoJob = 
  createJobInfoFromConfiguration
    grepAction          -- action config
    "Simpson"           -- options
    namesList           -- input (Tuples)
    []                  -- input (Files)
    1                   -- number of splitters
    2                   -- number of mappers
    1                   -- number of reducers
    1                   -- number of results
    TOTRawTuple         -- type of the result (file of raw)
    
namesList :: [((),String)]
namesList
  = map (\d -> ((),d)) names
  where
  names =
   [("Abraham Simpson"),
    ("Apu Nahasapeemapetilon"),
    ("Waylon Smithers"),
    ("Barney Gumble"),
    ("Bart Simpson"),
    ("Carl Carlson"),
    ("Homer Simpson"),
    ("Jacqueline Bouvier"),
    ("Kirk van Houten"),
    ("Lenny Leonard"),
    ("Lionel Hutz"),
    ("Lisa Simpson"),
    ("Luann van Houten"),
    ("Maggie Simpson"),
    ("Manjula Nahasapeemapetilon"),
    ("Marge Simpson"),
    ("Martin Prince"),
    ("Maude Flanders"),
    ("Milhouse van Houten"),
    ("Moe Syzslak"),
    ("Montgomery Burns"),
    ("Ned Flanders"),
    ("Patty Bouvier"),
    ("Rod Flanders"),
    ("Selma Bouvier"),
    ("Seymour Skinner"),
    ("Todd Flanders"),
    ("Troy McClure")]
    

-- ----------------------------------------------------------------------------
-- DemoFiles
-- ----------------------------------------------------------------------------


createGrepDemoFiles :: FS.FileSystem -> IO ()
createGrepDemoFiles fs
  = do
    -- let c = S.BinaryFile (encode ("foo","a aa aaa b bb bbb"))
    -- let c = S.TextFile "harddisk file"
    let c = encode namesList
    FS.createFile "namesList.txt" c fs