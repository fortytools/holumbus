{-# OPTIONS #-}

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

    , emptyInverted0
    , emptyInvertedCompressed
    , emptyInvertedSerialized
    , emptyInvertedCSerialized
    , emptyInvertedOSerialized

    , Sizeof(..)
    , sizeofAttrsInverted

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

import qualified Codec.Compression.BZip as BZ

import           Control.Arrow          ( second )
import           Control.DeepSeq

import qualified Data.ByteString.Lazy   as BS
import qualified Data.Binary            as B

import           Data.Function          ( on )

import           Data.Int

import           Data.List              ( foldl', sortBy )
import qualified Data.Map               as M
import           Data.Maybe

import           Holumbus.Index.Common
import           Holumbus.Index.Compression

import qualified Holumbus.Data.PrefixTree       as PT

import           Text.XML.HXT.Core      ( PU
                                        , XmlPickler
                                        , xpickle
                                        , xpAttr
                                        , xpElem
                                        , xpList
                                        , xpPair
                                        , xpText
                                        , xpWrap
                                        )

-- import           Debug.Trace            ( trace )

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
-- overloaded function sizeof for estimating the size of a value

class Sizeof a where
  sizeof        :: a -> Int64

-- ----------------------------------------------------------------------------
--
-- auxiliary types

newtype ByteString      = Bs { unBs :: BS.ByteString }
                          deriving (Eq, Show)

instance NFData ByteString where
  rnf s                 = BS.length (unBs s) `seq` ()

instance B.Binary ByteString where
  put                   = B.put . unBs
  get                   = B.get >>= return . Bs

instance Sizeof ByteString where
  sizeof                = (8 + ) . BS.length . unBs     -- we need the size of the length + the length

-- ----------------------------------------------------------------------------
--
-- the pure occurrence type, just wrapped in a newtype for instance declarations

newtype Occ0            = Occ0 { unOcc0 :: Occurrences }

instance ComprOccurrences Occ0 where
  fromOccurrences       = Occ0
  toOccurrences         = unOcc0

instance NFData Occ0 where
  rnf                   = rnf . unOcc0

instance B.Binary Occ0 where
  put                   = B.put . unOcc0
  get                   = B.get >>= return . Occ0

instance Sizeof Occ0 where
  sizeof                = BS.length . B.encode . unOcc0

-- ----------------------------------------------------------------------------
--
-- the simple-9 compressed occurrence type, just wrapped in a newtype for instance declarations

newtype OccCompressed   = OccCp { unOccCp :: CompressedOccurrences }
                          deriving (Eq, Show)

instance ComprOccurrences OccCompressed where
  fromOccurrences       = OccCp . deflateOcc
  toOccurrences         = inflateOcc . unOccCp

instance NFData OccCompressed where
  rnf                   = rnf . unOccCp

instance B.Binary OccCompressed where
  put                   = B.put . unOccCp
  get                   = B.get >>= return . OccCp

instance Sizeof OccCompressed where
  sizeof                = BS.length . B.encode . unOccCp

-- ----------------------------------------------------------------------------
--
-- the simpe-9 compresses occurrences serialized into a byte strings

newtype OccSerialized   = OccBs { unOccBs :: ByteString }
                          deriving (Eq, Show)

instance ComprOccurrences OccSerialized where
  fromOccurrences       = OccBs . Bs . B.encode . deflateOcc
  toOccurrences         = inflateOcc . B.decode . unBs . unOccBs

instance NFData OccSerialized where
  rnf                   = rnf . unOccBs

instance B.Binary OccSerialized where
  put                   = B.put . unOccBs
  get                   = B.get >>= return . OccBs

instance Sizeof OccSerialized where
  sizeof                = sizeof . unOccBs

-- ----------------------------------------------------------------------------
--
-- the simple-9 compressed occurrences serialized and bzipped into a byte string

newtype OccCSerialized  = OccCBs { unOccCBs :: ByteString }
                          deriving (Eq, Show)

instance ComprOccurrences OccCSerialized where
  fromOccurrences       = OccCBs . Bs . BZ.compress . B.encode . deflateOcc
  toOccurrences         = inflateOcc . B.decode  . BZ.decompress . unBs . unOccCBs

instance NFData OccCSerialized where
  rnf                   = rnf . unOccCBs

instance B.Binary OccCSerialized where
  put                   = B.put . unOccCBs
  get                   = B.get >>= return . OccCBs

instance Sizeof OccCSerialized where
  sizeof                = sizeof . unOccCBs

-- ----------------------------------------------------------------------------
--
-- the pure occurrences serialized and bzipped into a byte string
-- this seems to be the best choice: compression is about 4% better then simple-9 & bzip
-- and lookup is about 8% better

newtype OccOSerialized  = OccOBs { unOccOBs :: ByteString }
                          deriving (Eq, Show)

instance ComprOccurrences OccOSerialized where
  fromOccurrences       = OccOBs . Bs . BZ.compress . B.encode
  toOccurrences         = B.decode . BZ.decompress . unBs . unOccOBs

instance NFData OccOSerialized where
  rnf                   = rnf . unOccOBs

instance B.Binary OccOSerialized where
  put                   = B.put . unOccOBs
  get                   = B.get >>= return . OccOBs

instance Sizeof OccOSerialized where
  sizeof                = sizeof . unOccOBs

-- ----------------------------------------------------------------------------

-- | The index consists of a table which maps documents to ids and a number of index parts.

newtype Inverted occ    = Inverted
                          { unInverted :: Parts  occ    -- ^ The parts of the index, each representing one context.
                          } 
                          deriving (Show, Eq)

-- | The index parts are identified by a name, which should denote the context of the words.

type Parts occ          = M.Map Context (Part occ)

-- | The index part is the real inverted index. Words are mapped to their occurrences.
-- The part is implemented as a prefix tree

type Part occ           = PT.PrefixTree occ

-- ----------------------------------------------------------------------------

instance (NFData occ) => NFData (Inverted occ) where
    rnf         = rnf . unInverted

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

-- ----------------------------------------------------------------------------

instance (B.Binary occ, ComprOccurrences occ) => HolIndex (Inverted occ) where
  sizeWords                     = M.fold ((+) . PT.size) 0 . unInverted
  contexts                      = fmap fst . M.toList . unInverted

  allWords     i c              = fmap (second  toOccurrences) . PT.toList                      . getPart c $ i
  prefixCase   i c q            = fmap (second  toOccurrences) . PT.prefixFindWithKeyBF       q . getPart c $ i
  prefixNoCase i c q            = fmap (second  toOccurrences) . PT.prefixFindNoCaseWithKeyBF q . getPart c $ i
  lookupCase   i c q            = fmap ((,) q . toOccurrences) . maybeToList . PT.lookup      q . getPart c $ i
  lookupNoCase i c q            = fmap (second  toOccurrences) . PT.lookupNoCase              q . getPart c $ i

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
removeDocIdsPart ids            = PT.foldWithKey removeDocIds PT.empty
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

sizeofAttrsInverted             :: (Sizeof i) => Inverted i -> Int64
sizeofAttrsInverted             = M.fold ((+) . sizeofPT) 0 . unInverted
                                  where
                                  sizeofPT = PT.fold ((+) . sizeof) 0

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

