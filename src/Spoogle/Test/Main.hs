-- ----------------------------------------------------------------------------

{- |
   Module     : Spoogle.Test.Main
   Copyright  : Copyright (C) 2007 Timo B. Hübel
   License    : MIT

   Maintainer : Timo B. Hübel
   Maintainer : t.h@gmx.info
   Stability  : experimental
   Portability: portable
   Version    : $Id$

   This module just bundles all Spoogle tests.

-}

-- ----------------------------------------------------------------------------

module Spoogle.Test.Main where

import System
import Test.HUnit

import qualified Spoogle.Test.PatriciaTest as Patricia
import qualified Spoogle.Test.ParserTest as Parser

allTests = TestList
           [ Patricia.allTests
           , Parser.allTests
           ]

main :: IO ()
main = do
         c <- runTestTT allTests
         putStrLn $ show c
         let errs = errors c
             fails = failures c
         System.exitWith (codeGet errs fails)

codeGet :: Int -> Int -> ExitCode
codeGet errs fails | fails > 0 = ExitFailure 2
                   | errs > 0  = ExitFailure 1
                   | otherwise = ExitSuccess