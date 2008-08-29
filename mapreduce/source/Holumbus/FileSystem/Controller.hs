-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.FileSystem.Controller
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1


-}

-- ----------------------------------------------------------------------------

module Holumbus.FileSystem.Controller
(
-- * Typeclass
  ControllerClass(..)
)
where

import           Prelude hiding (appendFile)

import qualified Data.Set as Set

import           Holumbus.Network.Site
import           Holumbus.Network.Communication
import qualified Holumbus.FileSystem.Storage as S



-- ----------------------------------------------------------------------------
-- Typeclass
-- ----------------------------------------------------------------------------


class ControllerClass c where
  
  closeController :: c -> IO ()
    
  -- getServerPort :: c -> ServerPort
    
  getFileSites :: S.FileId -> c -> IO (Set.Set SiteId)
  
  containsFile :: S.FileId -> c -> IO Bool
  
  getNearestNodePortWithFile :: S.FileId -> SiteId -> c -> IO (Maybe ClientPort)
  
  getNearestNodePortForFile :: S.FileId -> Integer -> SiteId -> c -> IO (Maybe ClientPort)
  
  
  -- only to be used by the nodes
  
  createFile :: S.FileId -> IdType -> c -> IO ()

  deleteFile :: S.FileId -> IdType -> c -> IO ()

  appendFile :: S.FileId -> IdType -> c -> IO ()