{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

{-

This is the entry point for this web server application.

-}

module Main where

import Snap.Extension.Server

import Application
import Site

main :: IO ()
main = quickHttpServe applicationInitializer site

