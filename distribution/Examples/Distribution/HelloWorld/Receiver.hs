-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus
  Copyright  : Copyright (C) 2009 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1
  
-}

-- ----------------------------------------------------------------------------

module Main(main) where

import           Holumbus.Distribution.DNode
import           Holumbus.Distribution.DStreamPort
import           Holumbus.Common.Logging

main :: IO ()
main
  = do
    -- we want some Holumbus specific logging
    initializeLogging []
    -- make a Haskell DNode, named "myReceiver" on the Port 7999
    -- this only needs to be called once during the runtime of the program
    _ <- initDNode $ (defaultDNodeConfig "myReceiver")
      { dnc_MinPort = (fromInteger 7999), dnc_MaxPort = (fromInteger 7999) }
    -- make a new DStream, named "myStream"
    -- this stream can be used until you close it with "closeDStream"
    stream <- newDStream "myStream"
    -- wait for the next message, read it print it out to stdout
    msg <- (receive stream)::(IO String)
    putStrLn msg
    -- we are behaving nicely and clean everything up before we leave
    closeDStream stream
    deinitDNode
