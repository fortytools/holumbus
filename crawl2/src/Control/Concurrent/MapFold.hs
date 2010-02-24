{-# OPTIONS -XBangPatterns #-}

-- ------------------------------------------------------------

{- |
   Module     : Control.Concurrent.MapFold
   Copyright  : Copyright (C) 2010 Uwe Schmidt
   License    : MIT

   Maintainer : Uwe Schmidt (uwe@fh-wedel.de)
   Stability  : experimental
   Portability: none portable

   A map-fold function for performing list folds in parallel.

-}

-- ------------------------------------------------------------

module Control.Concurrent.MapFold
    ( mapFold )
where

import Control.Concurrent

-- ------------------------------------------------------------

mapFold			:: Int -> (a -> IO b) -> (b -> b -> IO b) -> [a] -> IO b
mapFold n m f xs@(_:_)	= do
			  c <- newChan
			  p <- newQSem n
			  mapFold' p c m f xs
mapFold _ _ _ []	= error "mapFold: empty list of arguments"

mapFold'		:: QSem -> Chan b -> (a -> IO b) -> (b -> b -> IO b) -> [a] -> IO b
mapFold' p c m f xs	= do
			  mapM_ (forkWorker m) xs
			  foldResults (length xs)
    where
    forkWorker m' x	= forkIO process
			  >> return ()
	where
	process		= do
			  -- logg   "started, wait for processor"
			  waitQSem p
			  -- logg $ "processing: " ++ show x
			  !res <- m' x

			  -- logg $ "done, result: " ++ show res

			  writeChan c res
			  signalQSem p
			  -- logg   "finished, processor released"

    foldResults n
	| n == 1	= do
			  -- logg $ "foldResults: collecting final result"
			  res <- readChan c
			  -- logg $ "foldResults: final result: " ++ show res
			  return res
	| otherwise	= do
			  -- logg $ "foldResults: " ++ show n ++ " remaining folds"
			  r1 <- readChan c
			  -- logg $ "foldResults: 1. arg: " ++ show r1
			  r2 <- readChan c
			  -- logg $ "foldResults: 2. arg: " ++ show r2
			  forkWorker (uncurry f) (r1, r2)
			  foldResults (n - 1)

-- ------------------------------------------------------------

