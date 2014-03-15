{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Hunt.ApiDocument
where

import           Data.Digest.Murmur64
import qualified Data.Map.Strict              as SM
import qualified Data.Text                    as T

import           Hayoo.FunctionInfo
import           Hayoo.Hunt.IndexSchema
import           Hayoo.PackageInfo

import           Holumbus.Crawler.IndexerCore
import           Holumbus.Query.Result        (Score)

import           Hunt.Common.ApiDocument
import           Hunt.Common.BasicTypes

-- ------------------------------------------------------------

toApiDoc :: ToDescr c => (URI, RawDoc c) -> ApiDocument
toApiDoc (uri, (rawContexts, rawTitle, rawCustom))
    = ApiDocument
      { apiDocUri      = uri
      , apiDocIndexMap = SM.fromList . concatMap toCC $ rawContexts
      , apiDocDescrMap = ( if null rawTitle
                           then id
                           else SM.insert d'name (T.pack rawTitle)
                         ) $ toDescr rawCustom
      }
    where
      toCC (_,  []) = []
      toCC (cx, ws) = [(cxToHuntCx cx, T.pack . unwords . map fst $ ws)]

boringApiDoc :: ApiDocument -> Bool
boringApiDoc a
    = SM.null (apiDocIndexMap a) && SM.null (apiDocDescrMap a)

chgIndexMap :: (SM.Map Context Content -> SM.Map Context Content) -> ApiDocument -> ApiDocument
chgIndexMap f a = a { apiDocIndexMap = f $ apiDocIndexMap a }

chgDescrMap :: (Description -> Description) -> ApiDocument -> ApiDocument
chgDescrMap f a = a { apiDocDescrMap = f $ apiDocDescrMap a }

insIndexMap :: Context -> Content -> ApiDocument -> ApiDocument
insIndexMap cx ct
    | T.null ct = id
    | otherwise = chgIndexMap $ SM.insert cx ct

insDescrMap :: T.Text -> T.Text -> ApiDocument -> ApiDocument
insDescrMap k v
    | T.null v  = chgDescrMap $ SM.delete k
    | otherwise = chgDescrMap $ SM.insert k v

lookupIndexMap :: Context -> ApiDocument -> T.Text
lookupIndexMap cx d
    = maybe "" id . SM.lookup cx . apiDocIndexMap $ d

-- ------------------------------------------------------------

-- auxiliary types for ToDescr instances

newtype FctDescr  = FD FunctionInfo
newtype PkgDescr  = PD PackageInfo
newtype RankDescr = RD Score

class ToDescr a where
    toDescr :: a -> Description

instance ToDescr a => ToDescr (Maybe a) where
    toDescr Nothing  = SM.empty
    toDescr (Just x) = toDescr x

instance ToDescr FctDescr where
    toDescr (FD x) = fiToDescr x

instance ToDescr RankDescr where
    toDescr (RD r) = mkDescr [(d'rank, rankToText r)]

instance ToDescr PkgDescr where
    toDescr (PD x) = piToDescr x

instance Hashable64 FctDescr where
    hash64Add (FD (FunctionInfo _mon sig pac sou fct typ))
        = hash64Add [sig, pac, sou, fct, show typ]

fiToHash :: String -> FunctionInfo -> Int
fiToHash name fi = fromInteger . fromIntegral . asWord64 . hash64Add name . hash64 . FD $ fi

-- ----------------------------------------

mkDescr :: [(T.Text, T.Text)] -> Description
mkDescr = SM.fromList . filter (not . T.null . snd)

fiToDescr :: FunctionInfo -> Description
fiToDescr (FunctionInfo mon sig pac sou fct typ)
    = mkDescr
      [ (d'module,      T.pack                   mon)
      , (d'signature,   T.pack . cleanupSig    $ sig)
      , (d'package,     T.pack                   pac)
      , (d'source,      T.pack                   sou)
      , (d'description, T.pack . cleanupDescr  $ fct)
      , (d'type,        T.pack . drop 4 . show $ typ)
      ]

piToDescr :: PackageInfo -> Description
piToDescr (PackageInfo nam ver dep aut mai cat hom syn des upl ran)
    = mkDescr
      [ (d'name,         T.pack nam)
      , (d'version,      T.pack ver)
      , (d'dependencies, T.pack dep)
      , (d'author,       T.pack aut)
      , (d'maintainer,   T.pack mai)
      , (d'category,     T.pack cat)
      , (d'homepage,     T.pack hom)
      , (d'synopsis,     T.pack syn)
      , (d'description,  T.pack des)
      , (d'upload,       T.pack upl)
      , (d'rank,         rankToText ran)
      ]

rankToText :: Score -> T.Text
rankToText r
    | r == defPackageRank = T.empty
    | otherwise           = T.pack . show $ r

-- HACK: the old Hayoo index contains keywords in the signature for type of object
-- these are removed, the type is encoded in the type field

cleanupSig :: String -> String
cleanupSig ('!' : s) = s
cleanupSig "class"   = ""
cleanupSig "data"    = ""
cleanupSig "module"  = ""
cleanupSig "newtype" = ""
cleanupSig "type"    = ""
cleanupSig x         = x

-- some descriptions consist of the single char "&#160;" (aka. nbsp),
-- these are removed

cleanupDescr :: String -> String
cleanupDescr "&#160;" = ""
cleanupDescr d        = d

-- ------------------------------------------------------------
