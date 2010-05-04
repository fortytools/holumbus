module Hayoo.PackageArchive
where


import qualified Codec.Archive.Tar       	as Tar
import qualified Codec.Archive.Tar.Entry 	as Tar

import qualified Codec.Compression.GZip 	as GZip ( decompress )

import           Control.Arrow

import qualified Data.ByteString.Lazy 		as BS
import           Data.ByteString.Lazy  		( ByteString )
import           Data.List

import           System.Time

import           System.FilePath

-- ------------------------------------------------------------

latestPackages		:: ClockTime -> ByteString -> [String]
latestPackages since	=  nub . sort . map fst . filterPackages since . selectPackages

filterPackages		:: ClockTime -> [(String, (String, ClockTime))] -> [(String, (String, ClockTime))]
filterPackages since	= filter ((since <=) . snd . snd)

selectPackages		:: ByteString -> [(String, (String, ClockTime))]
selectPackages		= GZip.decompress
			  >>> Tar.read
			  >>> Tar.foldEntries (\ e -> (entryInfo e ++)) [] (const [])

entryInfo		:: Tar.Entry -> [(String, (String, ClockTime))]
entryInfo e
    | isFile e
      &&
      isCabal path	= [(package, (version, time))]
    | otherwise		= []
    where
    path		= Tar.entryPath e
    (version : package : _)
			= splitDirectories >>> reverse >>> tail $ path
    time                = Tar.entryTime >>> epochTimeToClockTime $ e
    isCabal		= takeExtension >>> (== ".cabal")
    isFile e'		= case Tar.entryContent e' of
			  Tar.NormalFile _ _	-> True
			  _			-> False

epochTimeToClockTime :: Tar.EpochTime -> ClockTime
epochTimeToClockTime e = TOD s (truncate (1000000000 * f))
    where (s,f) = properFraction (toRational e)


-- ------------------------------------------------------------
