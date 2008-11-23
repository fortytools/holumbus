-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Common.Utils
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Some nice functions, needed everywhere.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Common.Utils
( 

  decodeMaybe

-- , lookupList
-- , lookupMaybe

-- , cutMaybePair
, filterEmptyList
, setEmptyList
-- , filterBlanks

, prettyRecordLine
-- , prettyResultLine
-- , handleExitError
)
where

import           Data.Binary
import qualified Data.ByteString.Lazy as B
-- import qualified Data.Map as Map
import           Data.Maybe
import           Data.Char

-- import           System.Exit


-- | parses something from a maybe bytestring, if Nothing, then Nothing
decodeMaybe :: (Binary a) => Maybe B.ByteString -> Maybe a
decodeMaybe Nothing = Nothing
decodeMaybe (Just b) = (Just $ decode b)

{-
lookupMaybe :: (Ord k) => Map.Map k v -> Maybe k -> Maybe v
lookupMaybe _ Nothing = Nothing
lookupMaybe m (Just k) = Map.lookup k m
-}
{-
lookupList :: (Ord k) => Map.Map k v -> [k] -> [v]
lookupList m ks = mapMaybe (\k' -> lookupMaybe m $ Just k') ks
-}    
{-
cutMaybePair :: Maybe (Maybe a, Maybe b) -> (Maybe a, Maybe b)
cutMaybePair (Nothing) = (Nothing, Nothing)
cutMaybePair (Just p)  = p
-}

filterEmptyList :: Maybe [k] -> [k]
filterEmptyList (Nothing) = []
filterEmptyList (Just ls) = ls


setEmptyList :: [k] -> Maybe [k]
setEmptyList [] = Nothing
setEmptyList ls = Just ls 

{-
filterBlanks :: Maybe String -> Maybe String
filterBlanks (Nothing) = Nothing
filterBlanks (Just "") = Nothing
filterBlanks (Just s)  = if s' == "" then Nothing else (Just s)
  where
  s' = filter isPrint s 
-}


-- | For the nice output of key-value-pairs 
prettyRecordLine :: (Show b, Show a) => Int -> a -> b -> String
prettyRecordLine n a b = stra ++ (replicate diff ' ') ++ show strb
  where
   stra = show a
   strb = show b
   diff = n - length stra
  
   
{-
prettyResultLine :: (Show e, Show a, Show d) => Int -> Either e d -> a -> String
prettyResultLine n (Left e) _ = prettyRecordLine n "Error:" e
prettyResultLine n (Right r) a = prettyRecordLine n a r
-}

{-
handleExitError :: (Show e) => IO (Either e a) -> IO a
handleExitError res
  = do
    res' <- res
    case res' of
      (Left e) ->
        do
        putStrLn $ "ERROR: " ++ show e
        exitFailure
      (Right a) -> return a
-}   