-- ----------------------------------------------------------------------------
{- |
  Module     : Examples.MapReduce.Sort.Sort
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Examples.MapReduce.Sort.Sort
(
  sortAction
, sortActionMap
, namesList
, sortDemoJob
, createSortDemoFiles
)
where

--import           Data.Binary
import           Holumbus.Common.MRBinary

import           System.Log.Logger

import qualified Holumbus.FileSystem.FileSystem as FS

import qualified Holumbus.Data.KeyMap as KMap
import           Holumbus.MapReduce.Types


localLogger :: String
localLogger = "Examples.MapReduce.Sort.Sort"


-- ----------------------------------------------------------------------------
-- Distributed Sort
-- ----------------------------------------------------------------------------


mapSort :: ActionEnvironment -> () -> () -> (String, String) -> IO [(String, (String, String))]
mapSort _ _ _ v
  = do 
    infoM localLogger "mapSort"
    debugM localLogger $ "input: " ++ show v
    let res = [(snd v, v)]
    debugM localLogger $ "output: " ++ show res
    return res


reduceSort :: ActionEnvironment -> () -> String -> [(String, String)] -> IO (Maybe [(String, String)])
reduceSort _ _ k vs 
  = do
    infoM localLogger "reduce/combine Sort"
    debugM localLogger $ "input: " ++ k ++ " - " ++ show vs
    return (Just vs)


-- ----------------------------------------------------------------------------
-- Actions
-- ----------------------------------------------------------------------------

sortAction
  :: ActionConfiguration 
       ()                                          -- state
       () (String, String)                         -- k1, v1
       String (String, String)                     -- k2, v2
       (String, String)                            -- v3 == v2
       [(String, String)]                          -- v4
sortAction
  = (defaultActionConfiguration "SORT")
        { ac_Map     = Just mapAction
        , ac_Combine = Nothing
        , ac_Reduce  = Just reduceAction
        }
    where
      mapAction 
        = (defaultMapConfiguration mapSort)
      reduceAction
        = (defaultReduceConfiguration reduceSort)
        

sortActionMap :: ActionMap
sortActionMap
  = KMap.insert (readActionConfiguration sortAction) $
    KMap.empty


-- ----------------------------------------------------------------------------
-- DemoJob
-- ----------------------------------------------------------------------------

  
sortDemoJob :: JobInfo
sortDemoJob = 
  createJobInfoFromConfiguration
    sortAction -- action config
    ()                  -- options
    namesList           -- input (Tuples)
    []                  -- input (Files)
    1                   -- number of splitters
    2                   -- number of mappers
    1                   -- number of reducers
    1                   -- number of results
    TOTRawTuple         -- type of the result (file of raw)
    
namesList :: [((),(String, String))]
namesList
  = map (\d -> ((),d)) names
  where
  names =
   [("Abraham","Simpson"),
    ("Apu","Nahasapeemapetilon"),
    ("Waylon","Smithers"),
    ("Barney","Gumble"),
    ("Bart","Simpson"),
    ("Carl","Carlson"),
    ("Homer","Simpson"),
    ("Jacqueline","Bouvier"),
    ("Kirk","van Houten"),
    ("Lenny","Leonard"),
    ("Lionel","Hutz"),
    ("Lisa","Simpson"),
    ("Luann","van Houten"),
    ("Maggie","Simpson"),
    ("Manjula","Nahasapeemapetilon"),
    ("Marge","Simpson"),
    ("Martin","Prince"),
    ("Maude","Flanders"),
    ("Milhouse","van Houten"),
    ("Moe","Syzslak"),
    ("Montgomery","Burns"),
    ("Ned","Flanders"),
    ("Patty","Bouvier"),
    ("Rod","Flanders"),
    ("Selma","Bouvier"),
    ("Seymour","Skinner"),
    ("Todd","Flanders"),
    ("Troy","McClure")]
    

-- ----------------------------------------------------------------------------
-- DemoFiles
-- ----------------------------------------------------------------------------


createSortDemoFiles :: FS.FileSystem -> IO ()
createSortDemoFiles fs
  = do
    -- let c = S.BinaryFile (encode ("foo","a aa aaa b bb bbb"))
    -- let c = S.TextFile "harddisk file"
    let c = encode namesList
    FS.createFile "namesList.txt" c fs