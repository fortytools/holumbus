-- ----------------------------------------------------------------------------

{- |
  Module     : Main

  Maintainer : Thorben Guelck, Tobias Lueders, Mathias Leonhardt, Uwe Schmidt
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This is the entry point for this web server application.
-}

-- ----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Snap.Extension.Server

import Application
import Site

main :: IO ()
main = quickHttpServe applicationInitializer site

-- ----------------------------------------------------------------------------
