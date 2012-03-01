{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Index.HashedIndex
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

import           Holumbus.Crawler.Types
import           Holumbus.Crawler.IndexerCore
import           Holumbus.Crawler.Logger

import           Holumbus.Index.Common          ( Document(..)
                                                , Occurrences
                                                , fromList
                                                , toList
                                                , unionDocs
                                                , mergeIndexes
                                                )

import           Holumbus.Index.HashedDocuments ( Documents(..)
                                                , emptyDocuments
                                                )

import           Text.XML.HXT.Core

-- ------------------------------------------------------------

{- .1: direct use of prefix tree with simple-9 encoded occurences

   concerning efficiency this implementation is about the same as the 2. one,
   space and time are minimally better, the reason could be less code working with classes

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
-- {- remove "--" to coment out region

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

type SmallDocuments             = Documents

emptySmallDocuments             :: SmallDocuments a
emptySmallDocuments             = emptyDocuments

-- ------------------------------------------------------------

defragmentHolumbusState :: (Binary di) => HolumbusState di -> HolumbusState di
defragmentHolumbusState = id

-- ------------------------------------------------------------

mergeAndWritePartialRes' :: (MonadIO m, NFData i, Binary i) =>
                            (SmallDocuments i -> SmallDocuments i) -> [String] -> String -> m ()
mergeAndWritePartialRes' id' pxs out
    = do notice $ ["merge hashed partial doctables from"] ++ pxs
         mdocs <- mergeSmallDocs $ map (++ ".doc") pxs
         notice $ ["write merged hashed doctable to", out ++ ".doc"]
         liftIO $ encodeFile (out ++ ".doc") (id' mdocs)
         notice $ ["merge hashed partial indexes from"] ++ pxs
         mixs  <- mergeCompactIxs $ map (++ ".idx") pxs
         notice $ ["write merged hashed indexes to", out ++ ".idx"]
         liftIO $ encodeFile (out ++ ".idx") mixs
         notice $ ["merge partial hashed doctables and indexes done"]

mergeSmallDocs :: (MonadIO m, NFData i, Binary i) => [String] -> m (SmallDocuments i)
mergeSmallDocs []
    = return emptySmallDocuments
mergeSmallDocs (x : xs)
    = do docs <- mergeSmallDocs xs
         notice ["merge hashed documents from file", x]
         doc1 <- liftIO $ decodeFile x
         rnf doc1 `seq`
                 (return $ unionDocs docs doc1)

mergeCompactIxs :: (MonadIO m) => [String] -> m CompactInverted
mergeCompactIxs []
    = return emptyCompactInverted
mergeCompactIxs (x : xs)
    = do ixs <- mergeCompactIxs xs
         notice ["merge compact hashed index from file", x]
         ix1 <- liftIO $ decodeFile x
         rnf ix1 `seq`
                 (return $ mergeIndexes ix1 ixs)

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
        = do notice ["writing hashed document table into binary file", docFile]
             liftIO $ encodeFile docFile (ixs_documents state)
             notice ["writing hashed compressed inverted index into binary file", idxFile]
             liftIO $ encodeFile idxFile (inverted2compactInverted . ixs_index $ state)
             notice ["writing hashed search index files finished"]
    where
      docFile = out ++ ".doc"
      idxFile = out ++ ".idx"

-- ------------------------------------------------------------

writePartialIndex :: (NFData c, XmlPickler c, Binary c) =>
                     Bool -> FilePath -> CrawlerAction a (HolumbusState c) ()
writePartialIndex xout fn
    = modifyStateIO
      theResultAccu
      (writePartialIndex' xout fn)

writePartialIndex' :: (NFData c, XmlPickler c, Binary c) =>
                      Bool -> FilePath -> HolumbusState c -> IO (HolumbusState c)
writePartialIndex' xout out ixs
    = do writeSearchBin out ixs
         if xout
            then writeXml (out ++ ".xml") ixs
            else return ()
         return emptyHolumbusState

-- ------------------------------------------------------------

notice :: MonadIO m => [String] -> m ()
notice = noticeC "hashedIndex"

-- ------------------------------------------------------------
