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
import           Holumbus.Network.Communication


-- ----------------------------------------------------------------------------
-- Typeclass
-- ----------------------------------------------------------------------------

class NodeClass n where

  closeNode :: n -> IO ()

  createFile :: S.FileId -> S.FileContent -> n -> IO ()
  
  createFiles :: [(S.FileId,S.FileContent)] -> n -> IO ()

  appendFile :: S.FileId -> S.FileContent -> n -> IO ()

  deleteFile :: S.FileId -> Bool -> n -> IO ()

  copyFile :: S.FileId -> ClientPort -> n -> IO ()

  containsFile :: S.FileId -> n -> IO Bool

  getFileContent :: S.FileId -> n -> IO (Maybe S.FileContent)
  
  getMultiFileContent :: [S.FileId] -> n -> IO [(S.FileId,S.FileContent)]

  getFileData :: S.FileId -> n -> IO (Maybe S.FileData)

  getFileIds :: n -> IO [S.FileId]
