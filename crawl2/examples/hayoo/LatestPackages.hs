module Main
where

import qualified Data.ByteString.Lazy 		as BS
import           Data.List

import           Hayoo.PackageArchive

import           System.Environment
import           System.Time

-- ------------------------------------------------------------

main	:: IO ()
main	= do
	  [days] <- getArgs
	  now <- getClockTime
	  t <- BS.readFile "tmp/00-index.tar.gz"
	  putStrLn . intercalate "," . latestPackages (((read days)::Int) `daysAgo` now) $ t

daysAgo			:: Int -> ClockTime -> ClockTime
daysAgo days (TOD s f) = TOD (s - toInteger days * 24 * 60 * 60) f

-- ------------------------------------------------------------
