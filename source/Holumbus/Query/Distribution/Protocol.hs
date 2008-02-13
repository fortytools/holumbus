-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Distribution.Protocol
  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Definitions for the protocol between query clients and query servers.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Distribution.Protocol where

import Network

-- | The identification of a query server. Should take the form @hostname:port@ or just 
-- @hostname@ if the default port (4242) should be used.
type Server = String

-- | The header sent with a request (a space delimited list of strings).
type Header = [String]

-- | The query command.
queryCmd :: String
queryCmd = "QUERY"

-- | The add command.
addCmd :: String
addCmd = "ADD"

-- | The remove command.
removeCmd :: String
removeCmd = "REMOVE"

-- | The replace command.
replaceCmd :: String
replaceCmd = "REPLACE"

-- | The response code indicating success.
successCode :: String
successCode = "OK"

-- | The response code indicating failure.
failureCode :: String
failureCode = "FAIL"

-- | The default port for query servers.
defaultPort :: PortNumber
defaultPort = 4242
