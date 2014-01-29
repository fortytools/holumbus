{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Holumbus.Index.Inverted.CompressedPrefixMem
    ( Inverted(..)
    , Parts
    , Part
    , Inverted0
    , InvertedCompressed
    , InvertedSerialized
    , InvertedCSerialized
    , InvertedOSerialized

    , ComprOccurrences(..)

    , emptyInverted
    , emptyInverted0
    , emptyInvertedCompressed
    , emptyInvertedSerialized
    , emptyInvertedCSerialized
    , emptyInvertedOSerialized

    , mapOcc
    , zipOcc
    , emptyOcc
    , theOcc
    , nullOcc
    , unionOcc
    , diffOcc
    , insertPosOcc
    , deletePosOcc
    , updateDocIdOcc
    , deleteDocIds

    , removeDocIdsInverted
    )
where

import           Control.Arrow                  (second)
import           Control.DeepSeq

import qualified Data.Binary                    as B
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Short          as SS

import           Data.Function                  (on)

import           Data.List                      (foldl', sortBy)
import qualified Data.Map.Strict                as M
import           Data.Maybe
import           Data.Size
import qualified Data.StringMap.Strict          as PT
import           Data.Typeable

import qualified Holumbus.ByteStringCompression as BZ
import           Holumbus.Index.Common
import           Holumbus.Index.Compression

import           Text.XML.HXT.Core              (PU, XmlPickler, xpAttr, xpElem,
                                                 xpList, xpPair, xpText, xpWrap,
                                                 xpickle)

-- ----------------------------------------------------------------------------

compress   :: BL.ByteString -> BL.ByteString
compress   = BZ.compressBZipSmart

decompress :: BL.ByteString -> BL.ByteString
decompress = BZ.decompressBZipSmart

-- ----------------------------------------------------------------------------

class ComprOccurrences s where
  toOccurrences         :: s -> Occurrences
  fromOccurrences       :: Occurrences -> s

mapOcc                  :: (ComprOccurrences s) => (Occurrences -> Occurrences) -> s -> s
mapOcc f                = fromOccurrences . f . toOccurrences

zipOcc                  :: (ComprOccurrences s) => (Occurrences -> Occurrences -> Occurrences) -> s -> s -> s
zipOcc op x y           = fromOccurrences $ op (toOccurrences x)(toOccurrences y)

emptyOcc                :: (ComprOccurrences s) => s
emptyOcc                = fromOccurrences  $ emptyOccurrences

theOcc                  :: (ComprOccurrences s) => s -> Occurrences
theOcc                  = toOccurrences

nullOcc                 :: (ComprOccurrences s) => s -> Bool
nullOcc                 = (== emptyOccurrences) . toOccurrences         -- better nullOccurrences, but not yet there

unionOcc                :: (ComprOccurrences s) => Occurrences -> s -> s
unionOcc os             = mapOcc $ mergeOccurrences os

diffOcc                 :: (ComprOccurrences s) => Occurrences -> s -> s
diffOcc os              = mapOcc $ substractOccurrences os

insertPosOcc            :: (ComprOccurrences s) => DocId -> Position -> s -> s
insertPosOcc d p        = mapOcc $ insertOccurrence d p

deletePosOcc            :: (ComprOccurrences s) => DocId -> Position -> s -> s
deletePosOcc d p        = mapOcc $ deleteOccurrence d p

updateDocIdOcc          :: (ComprOccurrences s) => (DocId -> DocId) -> s -> s
updateDocIdOcc f        = mapOcc $ updateOccurrences f

deleteDocIds            :: (ComprOccurrences s) => Occurrences -> s -> s
deleteDocIds ids        = mapOcc $ flip diffOccurrences ids

-- ----------------------------------------------------------------------------
--
-- auxiliary type for strict Bytestrings
-- the use of the smart constructor assures that the Bytestring is
-- fully evaluated before constructing the value
--
-- This type is used in the variants working with compressed and/or serialised data
-- to represent occurences 'OccCompressed', 'OccSerialized',  'OccCserialized', OccOSerialized'
--
-- The constructor must not be called directly, only via mkBs
--
-- The SByteString is a candidate for a BS.ShortByteString available with bytestring 0.10.4,
-- then 5 machine words can be saved per value

newtype SByteString     = Bs { unSs :: SS.ShortByteString }
                          deriving (Eq, Show, Typeable)

mkBs                    :: BS.ByteString -> SByteString
mkBs s                  = Bs $!! SS.toShort s

unBs                    :: SByteString -> BS.ByteString
unBs                    = SS.fromShort . unSs

-- in mkBs the ByteString is physically copied into a ShortByteString
-- to avoid sharing data with other possibly very large strings
-- this can occur e.g in the Binary instance with get
-- Working with ShortByteString prevents fragmentation of the heap store,
-- ByteStrings can't be move in the heap (are PINNED), ShortByteString can.

instance NFData SByteString where
-- use default implementation: eval to WHNF, and that's sufficient

instance B.Binary SByteString where
  put                   = B.put . unBs
  get                   = B.get >>= return . mkBs

-- to avoid sharing the data with with the input the ByteString is physically copied
-- before return. This should be the single place where sharing is introduced,
-- else the copy must be moved to mSBs

instance Sizeable SByteString where
    dataOf                      = dataOf  . unSs
    bytesOf                     = bytesOf . unSs
    statsOf                     = statsOf . unSs

-- ----------------------------------------------------------------------------
--
-- the pure occurrence type, just wrapped in a newtype for instance declarations
-- and for forcing evaluation

newtype Occ0            = Occ0 { unOcc0 :: Occurrences }
                          deriving (Show, Typeable)

mkOcc0                  :: Occurrences -> Occ0
mkOcc0 os               = Occ0 $!! os

instance ComprOccurrences Occ0 where
  fromOccurrences       = mkOcc0
  toOccurrences         = unOcc0

instance NFData Occ0 where
-- use default implementation: eval to WHNF, and that's sufficient

instance B.Binary Occ0 where
  put                   = B.put . unOcc0
  get                   = B.get >>= return . mkOcc0

instance Sizeable Occ0 where
    dataOf              = dataOf  . unOcc0
    bytesOf             = bytesOf . unOcc0
    statsOf             = statsOf . unOcc0

-- ----------------------------------------------------------------------------
--
-- the simple-9 compressed occurrence type, just wrapped in a newtype for instance declarations

newtype OccCompressed   = OccCp { unOccCp :: CompressedOccurrences }
                          deriving (Eq, Show, Typeable)

mkOccCp                  :: Occurrences -> OccCompressed
mkOccCp os               = OccCp $!! deflateOcc os

instance ComprOccurrences OccCompressed where
  fromOccurrences       = mkOccCp
  toOccurrences         = inflateOcc . unOccCp

instance NFData OccCompressed where
-- use default implementation: eval to WHNF, and that's sufficient

instance B.Binary OccCompressed where
  put                   = B.put . unOccCp
  get                   = B.get >>= ((return . OccCp) $!!)

instance Sizeable OccCompressed where
    dataOf              = dataOf  . unOccCp
    bytesOf             = bytesOf . unOccCp
    statsOf             = statsOf . unOccCp

-- ----------------------------------------------------------------------------
--
-- the simpe-9 compresses occurrences serialized into a byte strings

newtype OccSerialized   = OccBs { unOccBs :: SByteString }
                          deriving (Eq, Show, NFData, Typeable)

instance ComprOccurrences OccSerialized where
  fromOccurrences       = OccBs . mkBs . BL.toStrict . B.encode . deflateOcc
  toOccurrences         = inflateOcc . B.decode . BL.fromStrict . unBs . unOccBs

instance B.Binary OccSerialized where
  put                   = B.put . unOccBs
  get                   = B.get >>= return . OccBs

instance Sizeable OccSerialized where
    dataOf              = dataOf  . unOccBs
    bytesOf             = bytesOf . unOccBs
    statsOf             = statsOf . unOccBs

-- ----------------------------------------------------------------------------
--
-- the simple-9 compressed occurrences serialized and bzipped into a byte string

newtype OccCSerialized  = OccCBs { unOccCBs :: SByteString }
                          deriving (Eq, Show, NFData, Typeable)

instance ComprOccurrences OccCSerialized where
  fromOccurrences       = OccCBs . mkBs . BL.toStrict . compress . B.encode . deflateOcc
  toOccurrences         = inflateOcc . B.decode  . decompress . BL.fromStrict . unBs . unOccCBs

instance B.Binary OccCSerialized where
  put                   = B.put . unOccCBs
  get                   = B.get >>= return . OccCBs

instance Sizeable OccCSerialized where
    dataOf              = dataOf  . unOccCBs
    bytesOf             = bytesOf . unOccCBs
    statsOf             = statsOf . unOccCBs

-- ----------------------------------------------------------------------------
--
-- the pure occurrences serialized and bzipped into a byte string
-- this seems to be the best choice: compression is about 4% better then simple-9 & bzip
-- and lookup is about 8% better

newtype OccOSerialized  = OccOBs { unOccOBs :: SByteString }
                          deriving (Eq, Show, NFData, Typeable)

instance ComprOccurrences OccOSerialized where
  fromOccurrences       = OccOBs . mkBs . BL.toStrict . compress . B.encode
  toOccurrences         = B.decode . decompress . BL.fromStrict . unBs . unOccOBs

instance B.Binary OccOSerialized where
  put                   = B.put . unOccOBs
  get                   = B.get >>= return . OccOBs

instance Sizeable OccOSerialized where
    dataOf              = dataOf  . unOccOBs
    bytesOf             = bytesOf . unOccOBs
    statsOf             = statsOf . unOccOBs

-- ----------------------------------------------------------------------------

-- | The index consists of a table which maps documents to ids and a number of index parts.

newtype Inverted occ    = Inverted
                          { unInverted :: Parts  occ    -- ^ The parts of the index, each representing one context.
                          }
                          deriving (Show, Eq, NFData, Typeable)

-- | The index parts are identified by a name, which should denote the context of the words.

type Parts occ          = M.Map Context (Part occ)

-- | The index part is the real inverted index. Words are mapped to their occurrences.
-- The part is implemented as a prefix tree

type Part occ           = PT.StringMap occ

-- ----------------------------------------------------------------------------

instance (ComprOccurrences occ) => XmlPickler (Inverted occ) where
  xpickle               =  xpElem "indexes" $
                           xpWrap (Inverted, unInverted) xpParts

xpParts                 :: (ComprOccurrences occ) => PU (Parts occ)
xpParts                 = xpWrap (M.fromList, M.toList) (xpList xpContext)
  where
  xpContext             = xpElem "part" (xpPair (xpAttr "id" xpText) xpPart)

xpPart                  :: (ComprOccurrences occ) => PU (Part occ)
xpPart                  = xpElem "index" (xpWrap (PT.fromList, PT.toList) (xpList xpWord))
  where
  xpWord                = xpElem "word" $
                          xpPair (xpAttr "w" xpText)
                                 (xpWrap (fromOccurrences, toOccurrences) xpOccurrences)

-- ----------------------------------------------------------------------------

instance (B.Binary occ) => B.Binary (Inverted occ) where
  put                   = B.put . unInverted
  get                   = B.get >>= return . Inverted

instance (Typeable occ, Sizeable occ) => Sizeable (Inverted occ) where
    dataOf              = dataOf  . unInverted
    bytesOf             = bytesOf . unInverted
    statsOf             = statsOf . unInverted

-- ----------------------------------------------------------------------------

instance (B.Binary occ, ComprOccurrences occ) => HolIndex (Inverted occ) where
  sizeWords                     = M.foldl' (\ x y -> x + PT.size y) 0 . unInverted
  contexts                      = fmap fst . M.toList . unInverted

  allWords     i c              = fmap (second  toOccurrences) .
                                  PT.toList .
                                  getPart c $ i

  prefixCase   i c q            = fmap (second  toOccurrences) .
                                  PT.toListShortestFirst .
                                  PT.prefixFilter q .
                                  getPart c $ i

  prefixNoCase i c q            = fmap (second  toOccurrences) .
                                  PT.toListShortestFirst .
                                  PT.prefixFilterNoCase q .
                                  getPart c $ i

  lookupCase   i c q            = fmap ((,) q . toOccurrences) .
                                  maybeToList .
                                  PT.lookup q .
                                  getPart c $ i

  lookupNoCase i c q            = fmap (second  toOccurrences) .
                                  PT.toList .
                                  PT.lookupNoCase q .
                                  getPart c $ i

  mergeIndexes                  = zipInverted $ M.unionWith      $ PT.unionWith (zipOcc mergeOccurrences)
  substractIndexes              = zipInverted $ M.differenceWith $ substractPart

  insertOccurrences c w o i     = mergeIndexes     i (singletonInverted c w o)          -- see "http://holumbus.fh-wedel.de/hayoo/hayoo.html#0:unionWith%20module%3AData.Map"
  deleteOccurrences c w o i     = substractIndexes i (singletonInverted c w o)

  toList                        = concatMap (uncurry convertPart) . toListInverted
                                  where
                                  convertPart c  = map (\(w, o) -> (c, w, toOccurrences o)) . PT.toList

  splitByContexts               = splitInverted
                                  . map ( (\ i -> (sizeWords i, i))
                                          . Inverted
                                          . uncurry M.singleton
                                        )
                                  . toListInverted

  splitByDocuments i            = splitInverted ( map (uncurry convert) $
                                                  toListDocIdMap $
                                                  unionsWithDocIdMap (M.unionWith (M.unionWith unionPos)) docResults
                                                )
    where
    docResults                  = map (\c -> resultByDocument c (allWords i c)) (contexts i)
    convert d cs                = foldl' makeIndex (0, emptyInverted) (M.toList cs)
      where
      makeIndex r (c, ws)       = foldl' makeOcc r (M.toList ws)
        where
        makeOcc (rs, ri) (w, p) = (sizePos p + rs, insertOccurrences c w (singletonDocIdMap d p) ri)

  splitByWords i                = splitInverted ( map (uncurry convert) .
                                                  M.toList .
                                                  M.unionsWith (M.unionWith mergeOccurrences) $ wordResults
                                                )
    where
    wordResults                 = map (\c -> resultByWord c (allWords i c)) (contexts i)
    convert w cs                = foldl' makeIndex (0, emptyInverted) (M.toList cs)
      where
      makeIndex (rs, ri) (c, o) = (rs + sizeOccurrences o, insertOccurrences c w o ri)

  updateDocIds f                = mapInverted (M.mapWithKey updatePart)
    where
    updatePart c                = PT.mapWithKey (updateOcc (f c))
    updateOcc f' w              = mapOcc $ updateOccurrences (f' w)

-- ----------------------------------------------------------------------------

-- | Return a part of the index for a given context.

getPart                         :: Context -> Inverted i -> Part i
getPart c                       = fromMaybe PT.empty . M.lookup c . unInverted

-- | Substract one index part from another.
substractPart                   :: (ComprOccurrences i) => Part i -> Part i -> Maybe (Part i)
substractPart p1 p2
    | PT.null res               = Nothing
    | otherwise                 = Just res
    where
    res                         = diffPart p1 p2

diffPart                        :: (ComprOccurrences i) => Part i -> Part i -> Part i
diffPart                        = PT.differenceWith subtractDiffLists
    where
    subtractDiffLists o1 o2
        | nullOcc res           = Nothing
        | otherwise             = Just res
        where
        res                     = zipOcc substractOccurrences o1 o2

removeDocIdsPart                :: (ComprOccurrences i) => Occurrences -> Part i -> Part i
removeDocIdsPart ids            = PT.foldrWithKey removeDocIds PT.empty
    where
    removeDocIds k occ acc
        | nullOcc occ'          = acc
        | otherwise             = PT.insert k occ' acc
        where
        occ'                    = deleteDocIds ids occ

-- ----------------------------------------------------------------------------

mapInverted                     :: (Parts i -> Parts i) -> Inverted i -> Inverted i
mapInverted f                   = Inverted . f . unInverted

zipInverted                     :: (Parts i -> Parts i -> Parts i) -> Inverted i -> Inverted i -> Inverted i
zipInverted op i1 i2            = Inverted $ op (unInverted i1) (unInverted i2)

emptyInverted                   :: Inverted i
emptyInverted                   = Inverted M.empty

toListInverted                  :: Inverted i -> [(Context, Part i)]
toListInverted                  = M.toList . unInverted

-- | Create an index with just one word in one context.

singletonInverted               :: (ComprOccurrences i) => Context -> String -> Occurrences -> Inverted i
singletonInverted c w o         = Inverted . M.singleton c . PT.singleton w . fromOccurrences $ o

-- | Remove DocIds from index

removeDocIdsInverted            :: (ComprOccurrences i) => Occurrences -> Inverted i -> Inverted i
removeDocIdsInverted ids        = mapInverted $ M.map (removeDocIdsPart ids)

-- ----------------------------------------------------------------------------
--
-- copied from Holumbus.Index.Inverted.Memory

splitInverted                   :: (B.Binary i, ComprOccurrences i) => [(Int, Inverted i)] -> Int -> [Inverted i]
splitInverted inp n             = allocate mergeIndexes stack buckets
  where
  buckets                       = zipWith const (createBuckets n) stack
  stack                         = reverse (sortBy (compare `on` fst) inp)

-- | Allocates values from the first list to the buckets in the second list.
allocate                        :: (a -> a -> a) -> [(Int, a)] -> [(Int, a)] -> [a]
allocate _ _ []                 = []
allocate _ [] ys                = map snd ys
allocate f (x:xs) (y:ys)        = allocate f xs (sortBy (compare `on` fst) ((combine x y):ys))
  where
  combine (s1, v1) (s2, v2)     = (s1 + s2, f v1 v2)

-- | Create empty buckets for allocating indexes.
createBuckets                   :: Int -> [(Int, Inverted i)]
createBuckets n                 = (replicate n (0, emptyInverted))

-- ----------------------------------------------------------------------------
--
-- the 5 variants for the inverted index as prefix tree,

-- | The pure inverted index implemented as a prefix tree without any space optimizations.
-- This may be taken as a reference for space and time measurements for the other index structures

type Inverted0                  = Inverted Occ0

-- | The inverted index with simple-9 encoding of the occurence sets

type InvertedCompressed         = Inverted OccCompressed

-- | The inverted index with serialized occurence maps with simple-9 encoded sets

type InvertedSerialized         = Inverted OccSerialized

-- | The inverted index with serialized occurence maps with simple-9 encoded sets
-- and with the serialized bytestrings compressed with bzip2

type InvertedCSerialized        = Inverted OccCSerialized

-- | The pure inverted index with serialized occurence maps
-- and with the serialized bytestrings compressed with bzip2, no simple-9 encoding.
-- This is the most space efficient index of the 5 variants, even a few percent smaller
-- then InvertedCSerialized, and a few percent faster in lookup

type InvertedOSerialized        = Inverted OccOSerialized

-- ----------------------------------------------------------------------------

emptyInverted0                  :: Inverted0
emptyInverted0                  = emptyInverted

emptyInvertedCompressed         :: InvertedCompressed
emptyInvertedCompressed         = emptyInverted

emptyInvertedSerialized         :: InvertedSerialized
emptyInvertedSerialized         = emptyInverted

emptyInvertedCSerialized        :: InvertedCSerialized
emptyInvertedCSerialized        = emptyInverted

emptyInvertedOSerialized        :: InvertedOSerialized
emptyInvertedOSerialized        = emptyInverted


-- ----------------------------------------------------------------------------

