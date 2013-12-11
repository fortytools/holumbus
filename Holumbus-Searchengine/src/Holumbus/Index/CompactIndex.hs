{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Index.CompactIndex
    ( Document
    , Documents
    , SmallDocuments

    , Inverted
    , emptyInverted
    , removeDocIdsInverted

    , CompactInverted
    , emptyCompactInverted
    , inverted2compactInverted

    , HolumbusState
    , HolumbusConfig
    , emptyHolumbusState
    , defragmentHolumbusState

    , emptyIndexerState
    , emptyDocuments

    , mergeAndWritePartialRes'
    , writeXml
    , writeBin
    , writeSearchBin
    , writePartialIndex
    )
where

import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad.Reader

import           Data.Binary

import           Data.Function.Selector                      ((.&&&.))

import           Holumbus.Crawler.IndexerCore
import           Holumbus.Crawler.Logger
import           Holumbus.Crawler.Types

import           Holumbus.Index.Common                       (Document (..),
                                                              Occurrences, defragmentDocIndex,
                                                              fromList,
                                                              mergeIndexes,
                                                              toList, unionDocs)
import           Holumbus.Index.CompactDocuments             (Documents (..),
                                                              emptyDocuments)
import           Holumbus.Index.CompactSmallDocuments        (SmallDocuments (..), docTable2smallDocTable)
import qualified Holumbus.Index.CompactSmallDocuments        as CSD

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

{- .1: direct use of prefix tree with simple-9 encoded occurences

   concerning efficiency this implementation is about the same as the 2.,
   space and time are minimally better, the reason could be less code working with classes

   PrefixMem is not longer supported

import           Holumbus.Index.Inverted.PrefixMem

-- -}
-- ------------------------------------------------------------

{- .2: indirect use of prefix tree with simple-9 encoded occurences via InvertedCompressed

   minimal overhead compared to .1
   but less efficient in time (1598s / 1038s) and space
   total mem use (2612MB / 2498MB) than .3

import qualified Holumbus.Index.Inverted.CompressedPrefixMem    as PM

type Inverted                   = PM.InvertedCompressed

emptyInverted                   :: Inverted
emptyInverted                   = PM.emptyInvertedCompressed

-- -}

-- ------------------------------------------------------------
-- {-

{- .3: indirect prefix tree without compression of position sets

   best of these 3 implementations

   implementations with serializations become much more inefficient
   in runtime and are not worth to be considered
-}

import qualified Holumbus.Index.Inverted.CompressedPrefixMem as PM

type Inverted                   = PM.Inverted0

emptyInverted                   :: Inverted
emptyInverted                   = PM.emptyInverted0

removeDocIdsInverted            :: Occurrences -> Inverted -> Inverted
removeDocIdsInverted            = PM.removeDocIdsInverted

type CompactInverted            = PM.InvertedOSerialized

emptyCompactInverted            :: CompactInverted
emptyCompactInverted            = PM.emptyInvertedOSerialized

inverted2compactInverted        :: Inverted -> CompactInverted
inverted2compactInverted        = fromList PM.emptyInvertedOSerialized . toList

-- -}
-- ------------------------------------------------------------

type HolumbusState  di          = IndexerState       Inverted Documents di
type HolumbusConfig di          = IndexCrawlerConfig Inverted Documents di

emptyHolumbusState              :: HolumbusState di
emptyHolumbusState              = emptyIndexerState emptyInverted emptyDocuments

flushHolumbusState              :: HolumbusState di -> HolumbusState di
flushHolumbusState hs           = hs { ixs_index     = emptyInverted
                                     , ixs_documents = emptyDocuments
                                                       { lastDocId = lastDocId . ixs_documents $ hs }
                                     }

emptySmallDocuments             :: SmallDocuments a
emptySmallDocuments             = CSD.emptyDocuments

-- ------------------------------------------------------------

defragmentHolumbusState         :: (Binary di) =>
                                   HolumbusState di -> HolumbusState di
defragmentHolumbusState IndexerState
              { ixs_index     = ix
              , ixs_documents = dt
              }                 = IndexerState
                                  { ixs_index     = ix'
                                  , ixs_documents = dt'
                                  }
    where
    (dt', ix')                  = defragmentDocIndex dt ix

-- ------------------------------------------------------------

mergeAndWritePartialRes' :: (MonadIO m, NFData i, Binary i) =>
                            (SmallDocuments i -> SmallDocuments i) -> [String] -> String -> m ()
mergeAndWritePartialRes' id' pxs out
    = do notice $ ["merge partial doctables from"] ++ pxs
         mdocs <- mergeSmallDocs $ map (++ ".doc") pxs
         notice $ ["write merged doctable to", out ++ ".doc"]
         liftIO $ encodeFile (out ++ ".doc") (id' mdocs)
         notice $ ["merge partial indexes from"] ++ pxs
         mixs  <- mergeCompactIxs $ map (++ ".idx") pxs
         notice $ ["write merged indexes to", out ++ ".idx"]
         liftIO $ encodeFile (out ++ ".idx") mixs
         notice $ ["merge partial doctables and indexes done"]

mergeSmallDocs :: (MonadIO m, NFData i, Binary i) => [String] -> m (SmallDocuments i)
mergeSmallDocs []
    = return emptySmallDocuments
mergeSmallDocs (x : xs)
    = do docs <- mergeSmallDocs xs
         notice ["merge small documents from file", x]
         doc1 <- liftIO $ decodeFile x
         return $! unionDocs docs doc1

-- old stuff
--         rnf doc1 `seq`
--                 (return $ unionDocs docs doc1)

mergeCompactIxs :: (MonadIO m) => [String] -> m CompactInverted
mergeCompactIxs []
    = return emptyCompactInverted
mergeCompactIxs (x : xs)
    = do ixs <- mergeCompactIxs xs
         notice ["merge compact index from file", x]
         ix1 <- liftIO $ decodeFile x
         return $! mergeIndexes ix1 ixs

-- old stuff
--         rnf ix1 `seq`
--                 (return $ mergeIndexes ix1 ixs)

-- ------------------------------------------------------------

writeXml :: (MonadIO m, XmlPickler a) => FilePath -> a -> m ()
writeXml xf v
    | xmlOut
        = do notice ["writing into XML file", xmlFile]
             liftIO $ runX (constA v
                            >>> hxtSetTraceAndErrorLogger WARNING
                            >>> xpickleDocument xpickle [withIndent yes] xmlFile
                           )
                        >> return ()
             notice ["writing XML finished"]
    | otherwise
        = notice ["no XML output"]
    where
    (xmlOut, xmlFile)
        | null xf               = (False, xf)
        | xf == "-"             = (True,  "")
        | otherwise             = (True,  xf)

writeBin :: (MonadIO m, Binary a) => FilePath -> a -> m ()
writeBin out v
    | null out
        = notice ["no binary output"]
    | otherwise
        = do notice ["writing into binary file", out]
             liftIO $ encodeFile out v
             notice ["writing binary data finished"]

writeSearchBin :: (Binary c, MonadIO m) => FilePath -> HolumbusState c -> m ()
writeSearchBin out state
    | null out
        = notice ["no search index written"]
    | otherwise
        = do notice ["writing small document table into binary file", docFile]
             liftIO $ encodeFile docFile (docTable2smallDocTable . ixs_documents $ state)
             notice ["writing compressed inverted index into binary file", idxFile]
             liftIO $ encodeFile idxFile (inverted2compactInverted . ixs_index $ state)
             notice ["writing search index files finished"]
    where
      docFile = out ++ ".doc"
      idxFile = out ++ ".idx"

-- ------------------------------------------------------------

writePartialIndex :: (XmlPickler c, Binary c) =>
                     Bool -> FilePath -> CrawlerAction a (HolumbusState c) ()
writePartialIndex xout fn
    = modifyStateIO
      (theResultAccu .&&&. theResultInit)
      (\ (r, _i) -> do r' <- writePartialIndex' xout fn r
                       return (r', r')
      )

{- the above code is a bit tricky:
   when crawling is done in parallel, then initial result is used as a unit value,
   when merging results. When a partial index is written out, the document id count
   must not be set back to its initial value, to avoid renumbering when merging then
   partial indexes. As a consequence, not only the result accu must be changed
   but also the initial value.

   When this is not done, the indexer runs fine when using the sequential merge,
   but when running the parallel one, the index ids will overlap.
-}

writePartialIndex' :: (XmlPickler c, Binary c) =>
                      Bool -> FilePath -> HolumbusState c -> IO (HolumbusState c)
writePartialIndex' xout out ixs
    = do writeSearchBin out ixs
         if xout
            then writeXml (out ++ ".xml") ixs
            else return ()
         return $! flushHolumbusState ixs

-- old stuff
--         let ixs' = flushHolumbusState ixs
--         rnf ixs' `seq`
--             return ixs'

-- ------------------------------------------------------------

notice :: MonadIO m => [String] -> m ()
notice = noticeC "compactIndex"

-- ------------------------------------------------------------
