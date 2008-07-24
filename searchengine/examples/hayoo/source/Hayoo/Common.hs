-- ----------------------------------------------------------------------------

{- |
  Module     : HayooHelper.hs
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Helper functions and types used by the Hayoo crawler and the
  Hayoo web search.

-}

-- ----------------------------------------------------------------------------

module Hayoo.Common where

import Data.Char
import Data.Binary

import qualified Data.Map as M

import Control.Monad hiding (join)

import Text.XML.HXT.Arrow

import Holumbus.Utility

-- | Additional information about a function.
data FunctionInfo = FunctionInfo 
  { moduleName :: String      -- ^ The name of the module containing the function, e.g. Data.Map
  , signature :: String       -- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool
  , sourceURI :: Maybe String -- ^ An optional URI to the online source of the function.
  } 
  deriving (Show, Eq)

instance XmlPickler FunctionInfo where
  xpickle = xpWrap (\(m, s, r) -> FunctionInfo m s r, \(FunctionInfo m s r) -> (m, s, r)) xpFunction
    where
    xpFunction = xpTriple xpModule xpSignature xpSource
      where -- We are inside a doc-element, therefore everything is stored as attribute.
      xpModule = xpAttr "module" xpText0
      xpSignature = xpAttr "signature" xpText0
      xpSource = xpOption (xpAttr "source" xpText0)

instance Binary FunctionInfo where
  put (FunctionInfo m s r) = put m >> put s >> put r
  get = liftM3 FunctionInfo get get get

-- | Normalizes a Haskell signature, e.g. @String -> Int -> Int@ will be transformed to 
-- @a->b->b@. All whitespace will be removed from the resulting string.
normalizeSignature :: String -> String
normalizeSignature = join "->" . (replaceTypes M.empty ['a'..'z']) . split "->" . filter (not . isSpace)
  where
  replaceTypes _ _ [] = []
  replaceTypes v t (x:xs) = let (nv, ut, rx) = replace in rx:(replaceTypes nv ut xs)
    where
    replace = let ut = [head t] in maybe (M.insert r ut v, tail t, ut) (\n -> (v, t, n)) (M.lookup r v)
      where r = stripWith (\c -> (c == '(') || (c == ')')) x

-- | Strip unneeded whitespace from a signature, e.g. @String -> Map k a -> Int@ will be transformed
-- to @String->Map k a->Int@.
stripSignature :: String -> String
stripSignature = sep "->" . lsep "(" . rsep ")" . sep "." . sep "=>"
  where
  sep s = join s . map strip . split s
  lsep s = join s . map stripl . split s
  rsep s = join s . map stripr . split s
