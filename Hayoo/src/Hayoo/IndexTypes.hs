{-# OPTIONS #-}

-- ------------------------------------------------------------

module Hayoo.IndexTypes
    ( module Hayoo.IndexTypes
    , Document
    , Documents
    , SmallDocuments
    , FunctionInfo(..)
    , PackageInfo(..)
    , Score

    , docTable2smallDocTable
    )
where

import           Control.Arrow
import           Control.DeepSeq

import           Data.Binary

import           Data.List                      ( foldl' )
import qualified Data.Map                       as M    ( lookup )
import           Data.Maybe

import           Hayoo.FunctionInfo
import           Hayoo.PackageInfo
import           Hayoo.PackageRank

import           Holumbus.Crawler
import           Holumbus.Crawler.IndexerCore

import qualified Holumbus.Data.PrefixTree       as PT

import           Holumbus.Index.Common      {-    ( Document(..)
                                                , Occurrences
                                                , toList, fromList
                                                , custom, editDocIds, removeById, toMap, updateDocIds', updateDocuments
                                                )-}
import           Holumbus.Index.CompactDocuments
                                                ( Documents(..)
                                                , emptyDocuments
                                                )

import           Holumbus.Index.CompactSmallDocuments
                                                ( SmallDocuments(..)
                                                , docTable2smallDocTable
                                                )

import           Holumbus.Query.Result          ( Score )

-- import           Debug.Trace

-- ------------------------------------------------------------

{- .1: direct use of prefix tree with simple-9 encoded occurences

   concerning efficiency this implementation is about the same as the 2. one,
   space and time are minimally better, the reason could be less code working with classes

import           Holumbus.Index.Inverted.PrefixMem

-}
-- ------------------------------------------------------------

{- .2: indirect use of prefix tree with simple-9 encoded occurences via InvertedCompressed

   minimal overhead compared to .1
   but less efficient in time (1598s / 1038s) and space
   total mem use (2612MB / 2498MB) than .3

import qualified Holumbus.Index.Inverted.CompressedPrefixMem    as PM

type Inverted                   = PM.InvertedCompressed

emptyInverted                   :: Inverted
emptyInverted                   = PM.emptyInvertedCompressed
-}

-- ------------------------------------------------------------

{- .3: indirect prefix tree without compression of position sets

   best of these 3 implementations

   implementations with serializations become much more inefficient
   in runtime and are not worth to be considered
-}
 
import qualified Holumbus.Index.Inverted.CompressedPrefixMem    as PM

type Inverted                   = PM.Inverted0

emptyInverted                   :: Inverted
emptyInverted                   = PM.emptyInverted0

removeDocIdsInverted            :: Occurrences -> Inverted -> Inverted
removeDocIdsInverted            = PM.removeDocIdsInverted

type CompactInverted            = PM.InvertedOSerialized

inverted2compactInverted        :: Inverted -> CompactInverted
inverted2compactInverted        = fromList PM.emptyInvertedOSerialized . toList

-- ------------------------------------------------------------

type HayooState  di             = IndexerState       Inverted Documents di
type HayooConfig di             = IndexCrawlerConfig Inverted Documents di

type HayooCrawlerState di       = CrawlerState (HayooState di)

emptyHayooState                 :: HayooState di
emptyHayooState                 = emptyIndexerState emptyInverted emptyDocuments

flushHayooState                 :: HayooState di -> HayooState di
flushHayooState hs              = hs { ixs_index     = emptyInverted
                                     , ixs_documents = emptyDocuments
                                                       { lastDocId = lastDocId . ixs_documents $ hs }
                                     }

-- ------------------------------------------------------------

getPkgNameFct                   :: Document FunctionInfo -> String
getPkgNameFct                   = package . fromJust . custom

getPkgNamePkg                   :: Document PackageInfo -> String
getPkgNamePkg                   = p_name . fromJust . custom

-- ------------------------------------------------------------

removePackages'                 :: (Binary di, NFData di) =>
                                   (Document di -> String) -> String -> [String] -> Bool -> IO (HayooState di)
removePackages' pkgName ixName pkgList defragment
                                = do
                                  ix <- decodeFile ixName
                                  let ix1  = removePack' pkgName pkgList ix
                                  let ix2  = if defragment
                                             then defragmentIndex ix1
                                             else ix1
                                  rnf ix2 `seq` return ix2

-- ------------------------------------------------------------

removePack'                     :: (Binary di) =>
                                   (Document di -> String) -> [String] ->
                                   HayooState di -> HayooState di
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

defragmentIndex                 :: (Binary di) =>
                                   HayooState di -> HayooState di
defragmentIndex IndexerState
              { ixs_index     = ix
              , ixs_documents = dt
              }                 = IndexerState
                                  { ixs_index     = ix'
                                  , ixs_documents = dt'
                                  }
    where
    (dt', ix')                  = defragmentDocIndex dt ix
{-
    ix'                         = updateDocIds' editId ix
    ds'                         = editDocIds editId ds
    idMap                       = fromListDocIdMap . flip zip (map mkDocId [1..]) . keysDocIdMap . toMap $ ds
    editId i                    = fromJust . lookupDocIdMap i $ idMap
-}
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
                                = ixs { ixs_documents = updateDocuments insertRank ds }
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
-}

-- ------------------------------------------------------------

type RankTable                  = PT.PrefixTree Score

lookupRankTable                 :: String -> RankTable -> Score
lookupRankTable p               = fromMaybe 1.0 . PT.lookup p

buildRankTable                  :: SmallDocuments PackageInfo -> RankTable
buildRankTable                  = toMap
                                  >>> elemsDocIdMap
                                  >>> map ( custom
                                            >>> fromJust
                                            >>> (p_name &&& p_rank)
                                          )
                                  >>> PT.fromList 

-- ------------------------------------------------------------

type HayooIndexerState                  = HayooState   FunctionInfo
type HayooIndexerConfig                 = HayooConfig  FunctionInfo

type HayooIndexerCrawlerState           = CrawlerState HayooIndexerState

-- ------------------------------------------------------------

type HayooPkgIndexerState               = HayooState   PackageInfo
type HayooPkgIndexerConfig              = HayooConfig  PackageInfo

type HayooPkgIndexerCrawlerState        = CrawlerState HayooPkgIndexerState

-- ------------------------------------------------------------

