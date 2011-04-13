  -- ----------------------------------------------------------------------------

{- |
  Module     : Main
  Copyright  : Copyright (C) 2010-2011 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Main program to use as standalone webserver for serving Hayoo!

-}

-- ----------------------------------------------------------------------------

module Main where

import Hayoo.Search.Application

import Hack.Handler.SimpleServer

-- ----------------------------------------------------------------------------

-- | Maybe read these from the command line ... somewhen
ixBase :: FilePath
ixBase		= "./lib"

-- | The main application, fire up the server here!
main :: IO ()
main = do
       apl <- hayooInit ixBase
       run 4242 $ apl

-- ----------------------------------------------------------------------------
