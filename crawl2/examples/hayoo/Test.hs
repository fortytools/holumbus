{-# OPTIONS #-}

-- ------------------------------------------------------------

module Main
where

import           Control.DeepSeq

import qualified Data.Binary			as B
import           Data.Char
import           Data.Function
import		 Data.Function.Selector		( update, getS )
import           Data.List                      -- ( nub, sortBy )
import           Data.Maybe

import           Hayoo.HackagePackage
import           Hayoo.Haddock
import		 Hayoo.IndexConfig
import           Hayoo.IndexTypes
import           Hayoo.PackageArchive
import           Hayoo.URIConfig

import           Holumbus.Index.Common
import		 Holumbus.Crawler
import		 Holumbus.Crawler.CacheCore
import		 Holumbus.Crawler.IndexerCore

import 		 System.FilePath
import 		 System.CPUTime

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import		 System.IO

import		 Text.XML.HXT.Arrow		hiding ( readDocument )
import           Text.XML.HXT.Arrow.XmlCache

-- ------------------------------------------------------------

main	:: IO ()
main		= do
		  ix <- loadCompactInverted "ix.bin.idx"
		  wordsIx ix
		  countOccIx ix
		  return ()

-- ------------------------------------------------------------

loadCompactInverted	:: String -> IO CompactInverted
loadCompactInverted	= loadIx

-- ------------------------------------------------------------

loadIx			:: (NFData i, HolIndex i) => FilePath -> IO i
loadIx f		= withCpuTime $
                          do
                          msg $ "load index from binary file " ++ show f
                          ix <- loadFromBinFile f
                          rnf ix `seq` msg $ "loading finished"
                          return ix

-- ------------------------------------------------------------

wordsIx			:: (HolIndex i) => i -> IO ()
wordsIx	ix		= withCpuTime $
			  do
                          msg $ "number of words in index: " ++ show (sizeWords ix)
                          -- msg $ "words in index:\n" ++ unwords allW
    where
    allW		= [w |  c <- contexts ix, (w, _) <- allWords ix c]

countOccIx		:: (HolIndex i) => i -> IO ()
countOccIx ix		= withCpuTime $
                          do
                          msg $ "count occurrences by looking up all words once"
                          msg $ "index contains " ++ show wc ++ " words with " ++ show oc ++ " occurrences"
    where
    cws 		= [(c, w) | c <- contexts ix, (w, _) <- allWords ix c]
    oc                  = sum [ sizeOccurrences os | (c, w) <- cws, (_, os) <- lookupNoCase ix c w]
    wc 			= sizeWords ix 

-- ----------------------------------------

msg                     :: String -> IO ()
msg                     = hPutStrLn stderr

-- ------------------------------------------------------------

withCpuTime		:: IO a -> IO a
withCpuTime p		= do
                          start <- getCPUTime
                          res   <- p
                          end   <- getCPUTime
                          msg $ "cpu time for last action: " ++ showTime (end - start) ++ " sec."
                          return res

showTime		:: Integer -> String
showTime		= fill ' ' 10 . addDot 3 . fill '0' 4 . show . msec
    where
    five8               = 5 * (10::Integer)^(8::Int)
    ten9                = 2 * five8
    msec                :: Integer -> Integer
    msec t		= (t + five8) `div` ten9
    addDot n		= reverse . (\ (x, y) -> x ++ "." ++ y) . splitAt n . reverse
    fill c n s
        | length s < n	= reverse . take n . (++ replicate n c) . reverse $ s
        | otherwise	= s

-- ----------------------------------------

-- ----------------------------------------

type Key = [Char]
type Sym = Char

type KeyParser		= KeySet -> KeySet
newtype KeySet		= KS { unKS :: [(Sym, KeySet)] }
    deriving (Show)


mapKS			:: ([(Sym, KeySet)] -> [(Sym, KeySet)]) -> KeySet -> KeySet
mapKS f			= KS . f . unKS

-- ----------------------------------------
{-
-- | /O(max(L,R))/ Find all values where the string is a prefix of the key.

prefixFindNoCaseWithKey		:: Key -> PrefixTree a -> [(Key, a)] 
prefixFindNoCaseWithKey k	= toList . cutPx' (similarKeys kpIgnoreCase k)

prefixFindNoCase		:: Key -> PrefixTree a -> [a] 
prefixFindNoCase k		= elems . cutPx' (similarKeys kpIgnoreCase k)

lookupNoCase			:: Key -> PrefixTree a -> [(Key, a)]
lookupNoCase k			= toList . cutAllPx' (similarKeys kpIgnoreCase k)

-- ----------------------------------------

similarKeys		:: KeyParser -> Key -> KeySet
similarKeys _kp []	= val () empty
similarKeys  kp k	= foldr melt empty $ kp k
    where
    melt (k', r') t'	= union' const (siseq k' (similarKeys kp r')) t'

similarKeys'		:: KeyParser -> Key -> [Key]
similarKeys' kp		= keys . similarKeys kp
-}
-- ----------------------------------------

-- | Identity key parser

mkKeySet		:: Key -> KeySet
mkKeySet []		= KS []
mkKeySet (x:xs)		= KS [(x, mkKeySet xs)]

kpIgnoreCase0		:: KeyParser
kpIgnoreCase0		= kpSym (\ x -> [toUpper x, toLower x])

kpDeep			:: KeyParser -> KeyParser
kpDeep kp		= mapKS (map (second kp)) . kp

kpSym			:: (Sym -> Key) -> KeyParser
kpSym f 		=  mapKS (concat . fmap toSyms)
    where
    toSyms (x, ks)	= fmap (\ c -> (c, ks)) . sort . nub . f $ x


{-
-- | Sequential combination of key parsers

kpSeq			:: KeyParser -> KeyParser -> KeyParser
kpSeq kp1 kp2 k		= concatMap (uncurry p2) $ kp1 k
    where
    p2 k' r'		= fmap (\ k'' -> (k'', r')) $ similarKeys' kp2 k'

-- | Key parser for case insignificat keys

kpIgnoreCase		:: KeyParser
kpIgnoreCase []		= []
kpIgnoreCase (x:xs)	= fmap (\ c -> ([c], xs)) . L.nub $ [toLower x, toUpper x]


-- | Key parser for german umlaut substituion
-- TODO: extend parser for iso-latin-1

kpUmlaut		:: KeyParser
kpUmlaut []		= []
kpUmlaut (x:xs)		= fmap (\ s -> (s,xs)) . ([x]:) . maybeToList . L.lookup x $ esc
    where
    esc = [ ('\196', "Ae")
          , ('\214', "Oe")
          , ('\220', "Ue")
          , ('\228', "ae")
          , ('\246', "oe")
          , ('\252', "ue")
          , ('\223', "ss")
          ]
-}
-- ----------------------------------------
{-
-- | Set of strings implemented as lazy prefix tree.
-- @type PrefixSet = PrefixTree ()@ is not feasable because of the strict fields in the PrefixTree definition

data PrefixSet			= PSempty
                                | PSelem  PrefixSet
                                | PSnext  Sym PrefixSet PrefixSet
                                  deriving (Show)

emptyPS				:: PrefixSet
emptyPS				= PSempty

elemPS				:: PrefixSet -> PrefixSet
elemPS s@(PSelem _)		= s
elemPS s			= PSelem s

elem0PS				:: PrefixSet
elem0PS				= elemPS emptyPS

nextPS				:: Sym -> PrefixSet -> PrefixSet -> PrefixSet
nextPS _ PSempty n		= n
nextPS s c       n		= PSnext s c n

lastPS				:: Sym -> PrefixSet -> PrefixSet
lastPS s c			= nextPS s c emptyPS

nullPS				:: PrefixSet -> Bool
nullPS PSempty			= True
nullPS _			= False

singlePS			:: Key -> PrefixSet
singlePS			= foldr (\ c r -> lastPS c r)          elem0PS

prefixPS			:: Key -> PrefixSet
prefixPS			= foldr (\ c r -> elemPS (lastPS c r)) elem0PS

unionPS				:: PrefixSet -> PrefixSet -> PrefixSet
unionPS PSempty ps2		= ps2
unionPS ps1     PSempty		= ps1

unionPS (PSelem ps1) (PSelem ps2)	= PSelem (unionPS ps1 ps2)
unionPS (PSelem ps1)         ps2	= PSelem (unionPS ps1 ps2)
unionPS         ps1  (PSelem ps2)	= PSelem (unionPS ps1 ps2)

unionPS ps1@(PSnext c1 s1 n1)
        ps2@(PSnext c2 s2 n2)
    | c1 < c2			= nextPS c1 s1 (unionPS n1 ps2)
    | c1 > c2			= nextPS c2 s2 (unionPS ps1 n2)
    | otherwise			= nextPS c1 (unionPS s1 s2) (unionPS n1 n2)

foldPS				:: (Key -> b -> b) -> b -> (Key -> Key) -> PrefixSet -> b
foldPS f r kf  PSempty		= r
foldPS f r kf (PSelem ps1) 	= let r' = foldPS f r kf ps1
                                  in
                                  f (kf []) r'
foldPS f r kf (PSnext c1 s1 n1)	= let r' = foldPS f r kf n1
                                  in
                                  foldPS f r' (kf . (c1:)) s1

foldWithKeyPS			:: (Key -> b -> b) -> b -> PrefixSet -> b
foldWithKeyPS f e		= foldPS f e id

elemsPS				:: PrefixSet -> [Key]
elemsPS				= foldWithKeyPS (:) []

fuzzyCharsPS			:: (Sym -> [Sym]) -> PrefixSet -> PrefixSet
fuzzyCharsPS f  PSempty		= PSempty
fuzzyCharsPS f (PSelem ps)	= PSelem $ fuzzyCharsPS f ps
fuzzyCharsPS f (PSnext c s n)	= unionPS ps1 (fuzzyCharsPS f n)
    where
    s' 				= fuzzyCharsPS f s
    cs				= sort . nub . f $ c
    ps1				= foldr (\ c' r' -> nextPS c' s' r') emptyPS $ cs

noCasePS			:: PrefixSet -> PrefixSet
noCasePS			= fuzzyCharsPS (\ x -> [toUpper x, toLower x])
                                      
noLowerCasePS			:: PrefixSet -> PrefixSet
noLowerCasePS			= fuzzyCharsPS (\ x -> [toUpper x, x])

-- ------------------------------------------------------------

e1 = singlePS "abc"
e2 = prefixPS "abc"
e3 = foldl unionPS emptyPS . map singlePS $ ["zeus","anna","anton","an"]

-- ------------------------------------------------------------
-}