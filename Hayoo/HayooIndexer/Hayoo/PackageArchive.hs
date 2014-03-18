{-# LANGUAGE OverloadedStrings #-}

module Hayoo.PackageArchive
where


import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import qualified Codec.Compression.GZip  as GZip (decompress)

import           Control.Applicative
import           Control.Arrow
import           Control.Monad           ()

import           Holumbus.Crawler        (match)

import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BL
import           Data.List

import           Network.Browser
import           Network.HTTP
import           Network.URI             (URI, parseURI)

import           System.FilePath
import           System.Time

-- ------------------------------------------------------------

updateArchiveFile       :: IO ()
updateArchiveFile
    = do res <- snd <$> getHTTP
         case rspCode res of
           (2,0,0) -> BL.writeFile cacheFile $ rspBody res
           _       -> ioError . userError . show $ res
         return ()
    where
      archiveUri = "http://hackage.haskell.org/packages/" ++ archiveFile

      getHTTP :: IO (URI, Response BL.ByteString)
      getHTTP = browse $ do
                  setOutHandler (const $ return ())
                  setAllowRedirects True
                  request $
                    mkRequest GET $
                    maybe (error "Hayoo.PackageArchive.updateArchiveFile: Nothing") id $
                    parseURI archiveUri

archiveFile :: String
archiveFile = "index.tar.gz"

cacheFile :: String
cacheFile  = "cache" </> ("00-" ++ archiveFile)

-- ------------------------------------------------------------

getNewPackages          :: Int -> IO [String]
getNewPackages since
    = do t <- secondsAgo
         a <- getArchiveFile
         return $ latestPackages t a
    where
    secondsAgo          :: IO ClockTime
    secondsAgo          = do
                          (TOD s _f) <- getClockTime
                          return $ TOD ( if since <= 0
                                         then 0
                                         else s - toInteger since
                                       ) 0

-- ------------------------------------------------------------

getRegexPackages        :: String -> IO [String]
getRegexPackages re
    = filterPkg <$> getArchiveFile
    where
      filterPkg = nub . sort . filter (match re) . map fst . selectPackages

getArchiveFile :: IO ByteString
getArchiveFile = BL.readFile $ cacheFile

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
