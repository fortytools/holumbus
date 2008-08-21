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
  Controller(..)
)
where

import           Prelude hiding (appendFile)

import qualified Data.Set as Set

import           Holumbus.Network.Site
import qualified Holumbus.FileSystem.Messages as M
import qualified Holumbus.FileSystem.Storage as S



-- ----------------------------------------------------------------------------
-- Typeclass
-- ----------------------------------------------------------------------------


class Controller c where
  
  closeController :: c -> IO ()
  
  -- only debug
  getFileIds :: Integer -> c -> IO ()
  
  getControllerRequestPort :: c -> M.ControllerRequestPort
  
  registerNode :: SiteId -> M.NodeRequestPort -> c -> IO (M.NodeId, c)
  
  unregisterNode :: M.NodeId -> c -> IO c
  
  getFileSites :: S.FileId -> c -> IO (Set.Set SiteId)
  
  containsFile :: S.FileId -> c -> IO Bool
  
  getNearestNodePortWithFile :: S.FileId -> SiteId -> c -> IO (Maybe M.NodeRequestPort)
  
  getNearestNodePortForFile :: S.FileId -> Integer -> SiteId -> c -> IO (Maybe M.NodeRequestPort)
  
  
  -- only to be used by the nodes
  
  createFile :: S.FileId -> M.NodeId -> c -> IO c

  deleteFile :: S.FileId -> M.NodeId -> c -> IO c

  appendFile :: S.FileId -> M.NodeId -> c -> IO c
  
  printDebug :: c -> IO ()