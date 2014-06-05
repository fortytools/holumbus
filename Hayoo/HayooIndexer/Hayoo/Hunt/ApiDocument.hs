{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Hunt.ApiDocument
where

import           Data.Digest.Murmur64
import qualified Data.Map.Strict              as SM
import           Data.Text                    (Text)
import qualified Data.Text                    as T

import           Hayoo.FunctionInfo
import           Hayoo.Hunt.IndexSchema
import           Hayoo.PackageInfo

import           Holumbus.Crawler.IndexerCore

import           Hunt.ClientInterface
import qualified Hunt.Common.DocDesc          as DD

-- ------------------------------------------------------------

toApiDoc :: ToDescr c => (URI, RawDoc c, Float) -> ApiDocument
toApiDoc (uri, (rawContexts, rawTitle, rawCustom), wght)
    = ( if null rawTitle
        then id
        else addDescription d'name (T.pack rawTitle)
      )
      . setDescription (toDescr rawCustom)
      . setIndex       (SM.fromList . concatMap toCC $ rawContexts)
      . setDocWeight   (if wght == 1.0 then noScore else mkScore wght)
      $ mkApiDoc uri
    where
      toCC (_,  []) = []
      toCC (cx, ws) = [(cxToHuntCx cx, T.pack . unwords . map fst $ ws)]

boringApiDoc :: ApiDocument -> Bool
boringApiDoc a
    = SM.null (adIndex a)
      && DD.null (adDescr a)
      && adWght a == noScore

-- ------------------------------------------------------------

-- auxiliary types for ToDescr instances

newtype FctDescr  = FD FunctionInfo
newtype PkgDescr  = PD PackageInfo
newtype RankDescr = RD () -- old: Score

class ToDescr a where
    toDescr :: a -> Description

instance ToDescr a => ToDescr (Maybe a) where
    toDescr Nothing  = emptyDescription
    toDescr (Just x) = toDescr x

instance ToDescr FctDescr where
    toDescr (FD x) = fiToDescr x

instance ToDescr RankDescr where
    toDescr (RD _) = emptyDescription

instance ToDescr PkgDescr where
    toDescr (PD x) = piToDescr x

instance Hashable64 FctDescr where
    hash64Add (FD (FunctionInfo _mon sig pac sou fct typ))
        = hash64Add [sig, pac, sou, fct, show typ]

fiToHash :: String -> FunctionInfo -> Int
fiToHash name fi = fromInteger . fromIntegral . asWord64 . hash64Add name . hash64 . FD $ fi

fiToPkg :: FunctionInfo -> Text
fiToPkg (FunctionInfo _mon _sig pac _sou _fct _typ)
    = T.pack pac

-- ----------------------------------------

fiToDescr :: FunctionInfo -> Description
fiToDescr (FunctionInfo mon sig pac sou fct typ)
    = mkDescription
      [ (d'module,      T.pack                   mon)
      , (d'signature,   T.pack . cleanupSig    $ sig)
      , (d'package,     T.pack                   pac)
      , (d'source,      T.pack                   sou)
      , (d'description, T.pack . cleanupDescr  $ fct)
      , (d'type,        T.pack . drop 4 . show $ typ)
      ]

piToDescr :: PackageInfo -> Description
piToDescr (PackageInfo nam ver dep aut mai cat hom syn des upl ran)
    = insDescription d'dependencies (map T.pack . words $ dep) -- add the list of dependencies
      $
      mkDescription                                            -- add all simple text attributes
      [ (d'name,         T.pack nam)
      , (d'version,      T.pack ver)
      , (d'author,       T.pack aut)
      , (d'maintainer,   T.pack mai)
      , (d'category,     T.pack cat)
      , (d'homepage,     T.pack hom)
      , (d'synopsis,     T.pack syn)
      , (d'description,  T.pack des)
      , (d'upload,       T.pack upl)
      , (d'type,         "package")
      , (d'rank,         rankToText ran)
      ]

rankToText :: Float -> Text
rankToText r
    | r == defPackageRank = T.empty
    | otherwise           = T.pack . show $ r

-- HACK: the old Hayoo index contains keywords in the signature for the type of objects
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
