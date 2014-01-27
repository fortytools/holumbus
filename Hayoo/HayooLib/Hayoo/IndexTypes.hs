{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.IndexTypes
    ( module Hayoo.IndexTypes
    , module Holumbus.Index.CompactIndex
    , FunctionInfo(..)
    , PackageInfo(..)
    , Score
    )
where

import           Control.Arrow
import           Control.DeepSeq

import           Data.Binary
import           Data.List                    (foldl')
import qualified Data.Map                     as M (lookup)
import           Data.Maybe
import qualified Data.StringMap               as SM

import           Hayoo.FunctionInfo
import           Hayoo.PackageInfo
import           Hayoo.PackageRank

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore
import           Holumbus.Index.Common        (Document (..), custom,
                                               elemsDocIdMap, emptyDocIdMap,
                                               emptyPos, foldWithKeyDocIdMap,
                                               insertDocIdMap, keysDocIdMap,
                                               removeById, toMap,
                                               updateDocuments)
import           Holumbus.Index.CompactIndex
import           Holumbus.Query.Result        (Score)

-- ------------------------------------------------------------

getPkgNameFct                   :: Document FunctionInfo -> String
getPkgNameFct                   = package . fromJust . custom

getPkgNamePkg                   :: Document PackageInfo -> String
getPkgNamePkg                   = p_name . fromJust . custom

-- ------------------------------------------------------------

removePackages'                 :: (Binary di, NFData di) =>
                                   (Document di -> String) -> String ->
                                   [String] -> Bool -> IO (HolumbusState di)
removePackages' pkgName ixName pkgList defragment
                                = do
                                  ix <- decodeFile ixName
                                  let ix1  = removePack' pkgName pkgList ix
                                  let ix2  = if defragment
                                             then defragmentHolumbusState ix1
                                             else ix1
                                  rnf ix2 `seq`
                                      return ix2

-- ------------------------------------------------------------

removePack'                     :: (Binary di) =>
                                   (Document di -> String) -> [String] ->
                                   HolumbusState di -> HolumbusState di
removePack' pkgName ps IndexerState
              { ixs_index     = ix
              , ixs_documents = ds
              }                 = IndexerState
                                  { ixs_index     = ix'
                                  , ixs_documents = ds'
                                  }
    where
                                                        -- collect all DocIds used in the given packages
    docIds                      = foldWithKeyDocIdMap checkDoc emptyDocIdMap . toMap $ ds
    checkDoc did doc xs
        | docPartOfPack         = insertDocIdMap did emptyPos xs
        | otherwise             =                             xs
        where
        docPartOfPack           = (`elem` ps) . pkgName $ doc

                                                        -- remove all DocIds from index
    ix'                         = removeDocIdsInverted docIds ix

                                                        -- restrict document table
    ds'                         = foldl' removeById ds $ keysDocIdMap docIds

-- ------------------------------------------------------------

-- | package ranking is implemented by the following algorithm
--
-- .1 the rank of a package not used by another package is 1.0
--
-- .2 the rank of a package used by other packages is 1.0 + 0.5 * sum of the ranks of the
--    directly dependent packages. Example: a depends on b, b depends on c, d depends on c:
--    rank a = 1.0, rank b = 1.5, rank c = 2.25, rank d = 1.0
--
-- .3 this leads to a ranking where rank base > 1000.0 and rank bytestring > 300. To
--    reduce the weight differences, the log to base 2 is taken instead of the direct value

packageRanking                  :: HayooPkgIndexerState -> HayooPkgIndexerState
packageRanking ixs@(IndexerState { ixs_documents = ds })
                                = ixs { ixs_documents = packageDocRanking ds }

packageDocRanking :: Documents PackageInfo -> Documents PackageInfo
packageDocRanking ds = updateDocuments insertRank ds
    where
    deflate                     = 0.5
    scale                       = (/10.0) . fromInteger . round . (*10) . (+1.0) . logBase 2
    rank                        = ranking deflate
                                  . dagFromList
                                  . map (\ p -> (getPackageName p, getPackageDependencies p))
                                  . map fromJust
                                  . filter isJust               -- all illegal package refs are filtered out (there are illegal refs)
                                  . map custom
                                  . elemsDocIdMap
                                  . toMap $ ds

    insertRank d                = d { custom = fmap insertRank' (custom d) }
        where
        insertRank' ci          = setPackageRank (scale . fromMaybe (1.0) . M.lookup (getPackageName ci) $ rank) ci

{-
traceNothing d
    | isJust . custom $ d       = d
    | otherwise                 = traceShow d $ d
-- -}

-- ------------------------------------------------------------

type RankTable                  = SM.StringMap Score

lookupRankTable                 :: String -> RankTable -> Score
lookupRankTable p               = fromMaybe 1.0 . SM.lookup p

buildRankTable                  :: HayooPkgDocuments -> RankTable
buildRankTable                  = toMap
                                  >>> elemsDocIdMap
                                  -- map checkCustom                    -- hunting fromJust errors
                                  >>> filter (custom >>> isJust)        -- the robust version
                                  >>> map ( custom
                                            >>> fromJust
                                            >>> (p_name &&& p_rank)
                                          )
                                  >>> SM.fromList
{-
    where
      checkCustom d =
          case custom d of
            Nothing -> error $ "buildRankTable: Doc without custom component: " ++ show d
            Just _  -> d
-- -}
-- ------------------------------------------------------------

type HayooFctDocuments                  = SmallDocuments  FunctionInfo
type HayooFctIndex                      = CompactInverted

type HayooIndexerState                  = HolumbusState   FunctionInfo
type HayooIndexerConfig                 = HolumbusConfig  FunctionInfo

type HayooIndexerCrawlerState           = CrawlerState HayooIndexerState

-- ------------------------------------------------------------

type HayooPkgDocuments                  = SmallDocuments  PackageInfo
type HayooPkgIndex                      = CompactInverted

type HayooPkgIndexerState               = HolumbusState   PackageInfo
type HayooPkgIndexerConfig              = HolumbusConfig  PackageInfo

type HayooPkgIndexerCrawlerState        = CrawlerState HayooPkgIndexerState

-- ------------------------------------------------------------
