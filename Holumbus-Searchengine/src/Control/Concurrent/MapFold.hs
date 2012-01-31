{-# OPTIONS #-}

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
import Control.DeepSeq

-- ------------------------------------------------------------

mapFold                 :: (NFData b) => Int -> (a -> IO b) -> (b -> b -> IO b) -> [a] -> IO b
mapFold n m f xs@(_:_)  = do
                          c <- newChan
                          p <- newQSem n
                          mapFold' p c m f xs
mapFold _ _ _ []        = error "mapFold: empty list of arguments"

mapFold'                :: (NFData b) => QSem -> Chan b -> (a -> IO b) -> (b -> b -> IO b) -> [a] -> IO b
mapFold' p c m f xs     = do
                          mapM_ (forkWorker m) xs
                          foldResults (length xs)
    where
    forkWorker m' x     = forkIO process
                          >> return ()
        where
        process         = do
                          waitQSem p            -- request processor
                          res <- m' x           -- work
                          rnf res `seq`         -- force complete elvaluation
                              writeChan c res   -- deliver result
                          signalQSem p          -- release processor


    foldResults n
        | n == 1        = readChan c            -- get final result

        | otherwise     = do
                          r1 <- readChan c      -- get 1. arg
                          r2 <- readChan c      -- get 2. arg
                          forkWorker (uncurry f) (r1, r2)
                                                -- combine args and put result back into chanel
                          foldResults (n - 1)   -- continue

-- ------------------------------------------------------------

