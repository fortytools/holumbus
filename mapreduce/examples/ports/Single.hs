-- ----------------------------------------------------------------------------
{- |
  Module     : 
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}
-- ----------------------------------------------------------------------------

module Main(main) where

import Holumbus.Network.Port

type StringStream = Stream String

newStringStream :: IO StringStream
newStringStream = newStream


main :: IO ()
main
  = do
    putStrLn "Single-Demo Receiver"
    putStrLn "Begin"
    printStreamController
    stream <- newStringStream
    stream2 <- newStringStream
    port   <- newPort stream
    printStreamController
    putStrLn "Sending Hello World..."
    send port "Hello World"
    putStrLn "Reading Hello World..."
    msg <- readStreamMsg stream
    putStrLn "Result:"
    putStrLn $ show msg
    printStreamController
    closeStream stream
    printStreamController
    putStrLn "End"