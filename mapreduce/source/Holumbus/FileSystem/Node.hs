-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.FileSystem.Node
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}

-- ----------------------------------------------------------------------------

module Holumbus.FileSystem.Node
(
-- * Typeclass
  Node(..)

)
where

import qualified Holumbus.FileSystem.Messages as M
import qualified Holumbus.FileSystem.Storage as S



-- ----------------------------------------------------------------------------
-- Typeclass
-- ----------------------------------------------------------------------------

class Node n where

  closeNode :: n -> IO ()

  getNodeRequestPort :: n -> M.NodeRequestPort

  createFile :: S.FileId -> S.FileContent -> n -> IO n

  appendFile :: S.FileId -> S.FileContent -> n -> IO n

  deleteFile :: S.FileId -> Bool -> n -> IO n

  containsFile :: S.FileId -> n -> IO Bool

  getFileContent :: S.FileId -> n -> IO (Maybe S.FileContent)

  getFileData :: S.FileId -> n -> IO (Maybe S.FileData)

  getFileIds :: n -> IO [S.FileId]

  printDebug :: n -> IO ()