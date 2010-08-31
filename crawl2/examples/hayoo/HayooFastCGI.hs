  -- ----------------------------------------------------------------------------

{- |
  Module     : Main
  Copyright  : Copyright (C) 2010 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Main program to use as FastCGI app for serving Hayoo!

-}

-- ----------------------------------------------------------------------------

module Main where

import Hayoo.Search.Application

import Hack.Handler.FastCGI

-- ----------------------------------------------------------------------------

-- | Number of threads to use for serving requests
numThreads 	:: Int
numThreads 	= 4

-- | Maybe read these from the command line ... somewhen
ixBase :: FilePath
ixBase		= "."

-- | The main application, fire up the server here!
main :: IO ()
main = do
       apl <- hayooInit ixBase
       runFastCGIConcurrent numThreads apl

-- ----------------------------------------------------------------------------

