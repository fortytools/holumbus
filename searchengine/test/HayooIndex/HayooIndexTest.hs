{-# OPTIONS  -XBangPatterns #-}

module Main where

import           Control.DeepSeq

-- import           Data.Function
import           Data.Maybe
-- import           Data.Char
import qualified Data.Binary            as B

-- import qualified Data.List 			as L
-- import qualified Data.Map 			as M
-- import qualified Data.IntMap 			as IM
-- import qualified Data.IntSet 			as IS

-- import qualified Data.ByteString.UTF8 as B

-- import Network.URI (unEscapeString)

-- import Text.XML.HXT.Arrow
-- import Text.XML.HXT.DOM.Unicode

import Holumbus.Index.Inverted.OneFile		( Persistent )
import Holumbus.Index.Inverted.Memory		( Inverted, emptyInverted )

import Holumbus.Index.SmallDocuments		( SmallDocuments )
-- import Holumbus.Index.Cache
import Holumbus.Index.Common

-- import Holumbus.Query.Language.Grammar
-- import Holumbus.Query.Processor
-- import Holumbus.Query.Result
-- import Holumbus.Query.Ranking
-- import Holumbus.Query.Fuzzy

-- import Holumbus.Utility

import Hayoo.Common

import 		 System.IO
import 		 System.Environment
import 		 System.FilePath
-- import 		 System.Exit
-- import 		 System.Console.Editline.Readline
-- import 		 System.Console.GetOpt
import 		 System.CPUTime

import qualified Holumbus.Index.Inverted.CompressedPrefixMem as PX

-- ----------------------------------------

main			:: IO ()
main			= do
                          n <- getProgName
                          getProg n
    where
    getProg 		= fromMaybe main0 . flip lookup progs . takeFileName

progs			:: [(String, IO ())]
progs			= [ ("hayoo-persistent-to-memory", 	main1)
                          , ("hayoo-memory-to-prefixtree", 	main2)
                          , ("hayoo-count-occurrences", 	main3)
                          , ("hayoo-lookup-words",      	main4)
                          ]

main0			:: IO ()
main0			= do
                          n <- getProgName
                          msg $ "action " ++ show n ++ " not supported"
                          msg $ "expected: one of " ++ concatMap ((" " ++) . show . fst ) progs

-- ----------------------------------------
--
-- this action runs about 2 hours

main1			:: IO ()
main1			= withCpuTime $
                          do
       			  msg $ "converting " ++ show hixPersistent ++ " to " ++ show hixInverted
                          loadPersistentIndex hixPersistent >>=
                            writeInvertedIndex  hixInverted . convertIndex
                          msg $ "converting finished"

-- ----------------------------------------

main2	:: IO ()
main2			= do
      			  ix <- loadOrgIx
                          countOccIx ix

                          writeIx hixInverted0            PX.emptyInverted0           ix
                          writeIx hixInvertedCompressed   PX.emptyInvertedCompressed  ix
                          writeIx hixInvertedSerialized   PX.emptyInvertedSerialized  ix
                          writeIx hixInvertedCSerialized  PX.emptyInvertedCSerialized ix
                          writeIx hixInvertedOSerialized  PX.emptyInvertedOSerialized ix

-- ----------------------------------------

main3	:: IO ()
main3			= do
      			  ix <- loadOrgIx
                          countOccIx ix

                          attrSizeIx hixInverted0            PX.emptyInverted0           ix
                          attrSizeIx hixInvertedCompressed   PX.emptyInvertedCompressed  ix
                          attrSizeIx hixInvertedSerialized   PX.emptyInvertedSerialized  ix
                          attrSizeIx hixInvertedCSerialized  PX.emptyInvertedCSerialized ix
                          attrSizeIx hixInvertedOSerialized  PX.emptyInvertedOSerialized ix

-- ----------------------------------------

main4	:: IO ()
main4			= do
      			  ix <- loadOrgIx
                          countOccIx ix

                          loadIx hixInverted0            PX.emptyInverted0           >>= countOccIx
                          loadIx hixInvertedCompressed   PX.emptyInvertedCompressed  >>= countOccIx
                          loadIx hixInvertedSerialized   PX.emptyInvertedSerialized  >>= countOccIx
                          loadIx hixInvertedCSerialized  PX.emptyInvertedCSerialized >>= countOccIx
                          loadIx hixInvertedOSerialized  PX.emptyInvertedOSerialized >>= countOccIx

-- ----------------------------------------

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

loadOrgIx		:: IO Inverted
loadOrgIx		= loadIx hixInverted emptyInverted

-- ----------------------------------------

hixInverted		:: String
hixInverted		= "indexes/hayoo-index-inverted-memory.bin"

hixPersistent		:: String
hixPersistent		= "indexes/hayoo-index.bin"


hixInverted0		:: String
hixInverted0		= "indexes/hayoo-inverted-mem-occ.bin"

hixInvertedCompressed		:: String
hixInvertedCompressed	= "indexes/hayoo-inverted-mem-compressed-occ.bin"

hixInvertedSerialized		:: String
hixInvertedSerialized	= "indexes/hayoo-inverted-mem-compressed-serialized.bin"

hixInvertedCSerialized		:: String
hixInvertedCSerialized	= "indexes/hayoo-inverted-mem-compressed-serialized-bzip.bin"

hixInvertedOSerialized		:: String
hixInvertedOSerialized	= "indexes/hayoo-inverted-mem-serialized-bzip.bin"

-- ----------------------------------------

convertIndex		:: Persistent -> Inverted
convertIndex		= fromList emptyInverted . toList

loadPersistentIndex 	:: FilePath -> IO Persistent
loadPersistentIndex 	= loadFromFile

loadSmallDocuments 	:: FilePath -> IO (SmallDocuments FunctionInfo)
loadSmallDocuments 	= loadFromBinFile

msg                     :: String -> IO ()
msg                     = hPutStrLn stderr

-- ----------------------------------------

loadInvertedIndex 	:: FilePath -> IO Inverted
loadInvertedIndex 	= loadFromFile

writeInvertedIndex 	:: FilePath -> Inverted -> IO ()
writeInvertedIndex 	= writeToBinFile

-- ----------------------------------------

convertIx		:: (HolIndex i, HolIndex j) => i -> j -> i
convertIx e		= fromList e . toList

loadIx			:: (NFData i, HolIndex i) => FilePath -> i -> IO i
loadIx f e		= withCpuTime $
                          do
                          msg $ "load index from binary file " ++ show f
                          ix <- loadFromBinFile f
                          rnf ix `seq` msg $ "loading finished"
                          return (const' ix e)
                          where
                          const' :: c -> c -> c
                          const' = const
                        
-- ----------------------------------------

writeIx	 		:: (HolIndex i, HolIndex j) => FilePath -> i -> j -> IO ()
writeIx f e i		= withCpuTime $
                          do
                          msg $ "write index to binary file " ++ show f
                          writeToBinFile f . convertIx e $ i
                          msg $ "index written"

attrSizeIx		:: (PX.Sizeof i, B.Binary i, PX.ComprOccurrences i, HolIndex j) => String -> PX.Inverted i -> j -> IO ()
attrSizeIx s e i        = withCpuTime $
                          do
                          msg $ "computing sizeof index attributes " ++ show s
                          msg $ "size in bytes: " ++ (show $ PX.sizeofAttrsInverted $ convertIx e $ i)

wordsIx			:: (HolIndex i) => i -> IO ()
wordsIx	ix		= withCpuTime $
                          msg $ "number of words in index: " ++ show (sizeWords ix)


countOccIx		:: (HolIndex i) => i -> IO ()
countOccIx ix		= withCpuTime $
                          do
                          msg $ "count occurrences by looking up all words once"
                          msg $ "index contains " ++ show wc ++ " words with " ++ show oc ++ " occurrences"
    where
    cws 		= [(c, w) | c <- contexts ix, (w, _) <- allWords ix c]
    oc                  = sum [ sizeOccurrences os | (c, w) <- cws, (_, os) <- lookupCase ix c w]
    wc 			= sizeWords ix 

-- ----------------------------------------
