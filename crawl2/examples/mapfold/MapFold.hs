-- ------------------------------------------------------------

{- |
   Module     : MapFold
   Copyright  : Copyright (C) 2009 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: none portable

   Test program for Control.Concurrent.MapFold module
-}

-- ------------------------------------------------------------

module MapFold
where

import Control.Concurrent
import Control.Concurrent.MapFold

import System.IO
import System.IO.Unsafe
import System.Random

-- ------------------------------------------------------------

main 		:: IO ()
main		= do
                  mapM_ (runEx [1..10]) [1,2,5,10,20]

runEx 		:: [Int] -> Int -> IO ()
runEx xs n	= do
                  putStrLn $ "example run with " ++ show n ++ " processors and input " ++ show xs
                  res <- run n xs
                  putStrLn $ "result is " ++ res

-- ------------------------------------------------------------
--
-- test case: take a list of numbers, in the map phase convert them into strings
-- and in the fold phase build the expression for summing up the numbers.
-- The trace output shows the sequence of the map and fold operations performed.
-- The test runs show different results for different runs.
--
-- So in general the binary op
-- applied during the fold phase must be associative and symmetric to deliver useful
-- results (to make the result determinated).

run 		:: Int -> [Int] -> IO String
run processors	= mapFold processors mapF foldF
    where
    mapF x	= do
                  logg $ "mapF:  inp = " ++ show x
                  randomDelay
                  res <- return $ show x
                  logg $ "mapF:  res = " ++ show res
                  return res
    foldF x y	= do
                  logg $ "foldF: inp = " ++ show (x,y)
                  randomDelay
                  res <- return $ "(" ++ x ++ "+" ++ y ++ ")"
                  logg $ "foldF: res = " ++ show res
                  return res

-- ------------------------------------------------------------
--
-- simulate some compilcated computation
-- by delaying the process a fraction of a second

randomDelay	:: IO ()
randomDelay	= mysec >>= threadDelay
    where
    mysec	:: IO Int
    mysec	= do
                  r <- randomIO
                  return $ (r :: Int) `mod` 1000000

-- ------------------------------------------------------------
--
-- just for syncing log messages

stdErrSem		:: QSem
stdErrSem		= unsafePerformIO $ newQSem 1

waitStderr, signalStderr :: IO ()
waitStderr		= waitQSem stdErrSem
signalStderr		= signalQSem stdErrSem

logg			:: String -> IO ()
logg msg		= do
			  waitStderr
			  tid <- myThreadId
			  hPutStrLn stderr $ show tid ++ ": " ++ msg
			  hFlush    stderr
			  signalStderr

-- ------------------------------------------------------------
