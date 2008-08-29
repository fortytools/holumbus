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
  NodeClass(..)

)
where

import qualified Holumbus.FileSystem.Storage as S



-- ----------------------------------------------------------------------------
-- Typeclass
-- ----------------------------------------------------------------------------

class NodeClass n where

  closeNode :: n -> IO ()

  createFile :: S.FileId -> S.FileContent -> n -> IO ()

  appendFile :: S.FileId -> S.FileContent -> n -> IO ()

  deleteFile :: S.FileId -> Bool -> n -> IO ()

  containsFile :: S.FileId -> n -> IO Bool

  getFileContent :: S.FileId -> n -> IO (Maybe S.FileContent)

  getFileData :: S.FileId -> n -> IO (Maybe S.FileData)

  getFileIds :: n -> IO [S.FileId]