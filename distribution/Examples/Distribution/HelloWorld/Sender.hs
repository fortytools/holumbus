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
    -- make a Haskell DNode, we don't care about its name, so we leave it
    -- blank. The system will generate a unique random name on its own.
    -- this only needs to be called once during the runtime of the program
    _ <- initDNode $ defaultDNodeConfig ""
    -- we need to know how to address the receiver node, so we have to provide
    -- its address, this only needs to be done once, or when the address of
    -- the receiver changes (which will not happen in most applications)
    addForeignDNode $ mkDNodeAddress "myReceiver" "localhost" (fromInteger 7999)
    -- we make a new port, connected to "myStream" at the node "myReceiver"
    port <- newDPort "myStream" "myReceiver"
    -- send the messages
    send port "Hello World"
    -- we are behaving nicely and clean everything up before we leave
    deinitDNode
