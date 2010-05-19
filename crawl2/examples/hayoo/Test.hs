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
