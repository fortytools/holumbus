
module Holumbus.Common.Debug where


class Debug m where
  
  printDebug :: m -> IO ()