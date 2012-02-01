module Hayoo.PackageArchive
where


import qualified Codec.Archive.Tar              as Tar
import qualified Codec.Archive.Tar.Entry        as Tar

import qualified Codec.Compression.GZip         as GZip ( decompress )

import           Control.Arrow

import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy           as BS
import           Data.List

import           System.FilePath
import           System.Process                 ( system )
import           System.Time

-- ------------------------------------------------------------

getNewPackages          :: Bool -> Int -> IO [String]
getNewPackages updateArchive since
                        = do
                          if updateArchive
                             -- this is a hack, should be done more elegant without the use of system and wget
                             then system ("wget http://hackage.haskell.org/packages/" ++ archiveFile ++ " -O cache/" ++ archiveFile)
                                  >> return ()
                             else return ()
                          t <- secondsAgo
                          a <- BS.readFile $ "cache/" ++ archiveFile
                          return $ latestPackages t a
    where
    archiveFile         = "00-index.tar.gz"
    secondsAgo          :: IO ClockTime
    secondsAgo          = do
                          (TOD s _f) <- getClockTime
                          return $ TOD (s - toInteger since) 0

-- ------------------------------------------------------------

latestPackages          :: ClockTime -> ByteString -> [String]
latestPackages since    =  nub . sort . map fst . filterPackages since . selectPackages

filterPackages          :: ClockTime -> [(String, (String, ClockTime))] -> [(String, (String, ClockTime))]
filterPackages since    = filter ((since <=) . snd . snd)

selectPackages          :: ByteString -> [(String, (String, ClockTime))]
selectPackages          = GZip.decompress
                          >>> Tar.read
                          >>> Tar.foldEntries (\ e -> (entryInfo e ++)) [] (const [])

entryInfo               :: Tar.Entry -> [(String, (String, ClockTime))]
entryInfo e
    | isFile e
      &&
      isCabal path      = [(package, (version, time))]
    | otherwise         = []
    where
    path                = Tar.entryPath e
    (version : package : _)
                        = splitDirectories >>> reverse >>> tail $ path
    time                = Tar.entryTime >>> epochTimeToClockTime $ e
    isCabal             = takeExtension >>> (== ".cabal")
    isFile e'           = case Tar.entryContent e' of
                          Tar.NormalFile _ _    -> True
                          _                     -> False

epochTimeToClockTime :: Tar.EpochTime -> ClockTime
epochTimeToClockTime e = TOD s (truncate (1000000000 * f))
    where (s,f) = properFraction (toRational e)


-- ------------------------------------------------------------
