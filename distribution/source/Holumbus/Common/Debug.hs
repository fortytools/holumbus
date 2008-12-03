-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Common.Debug
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


  A typeclass for printing debug output.

-}
-- ----------------------------------------------------------------------------

module Holumbus.Common.Debug where


class Debug m where

  -- | Just print out some debug output.  
  printDebug :: m -> IO ()