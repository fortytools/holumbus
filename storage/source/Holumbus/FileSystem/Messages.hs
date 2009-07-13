-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.FileSystem.Messages
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This module contains all message types which are exchanged between node and
  Controller.
-}

-- ----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
module Holumbus.FileSystem.Messages
(
-- * Datatypes
  NodeId

-- * Message Types from and to the Controller
, ControllerRequestMessage(..)
, ControllerResponseMessage(..)

-- * Message Types from and to the Node
, NodeRequestMessage(..)
, NodeResponseMessage(..)

-- * request an response handling
, performPortAction -- reexport from Holumbus.Network.Messages
)
where

import           Prelude hiding (appendFile)

--import           Data.Binary
import Holumbus.Common.MRBinary
import           Data.Set

import           Holumbus.Network.Communication
import qualified Holumbus.Network.Site as Site
import qualified Holumbus.FileSystem.Storage as S
import           Holumbus.Network.Messages



-- ----------------------------------------------------------------------------
-- Datatypes
-- ---------------------------------------------------------------------------- 

type NodeId = Int



-- ----------------------------------------------------------------------------
-- Message Types from and to the Controller
-- ----------------------------------------------------------------------------


-- | Requests datatype, which is send to a filesystem Controller.
data ControllerRequestMessage
  = CReqContains                   S.FileId
  | CReqGetFileSites               S.FileId
  | CReqGetNearestNodePortWithFile S.FileId Site.SiteId 
  | CReqGetNearestNodePortForFile  S.FileId Integer Site.SiteId
  | CReqCreate                     S.FileId NodeId
  | CReqAppend                     S.FileId NodeId
  | CReqDelete                     S.FileId NodeId
  | CReqUnknown
  deriving (Show)


instance Binary ControllerRequestMessage where
  put (CReqContains f)                      = putWord8 1  >> put f
  put (CReqGetFileSites f)                  = putWord8 2  >> put f
  put (CReqGetNearestNodePortWithFile f s)  = putWord8 3  >> put f >> put s
  put (CReqGetNearestNodePortForFile f l s) = putWord8 4  >> put f >> put l >> put s
  put (CReqCreate f n)                      = putWord8 5  >> put f >> put n
  put (CReqAppend f n)                      = putWord8 6  >> put f >> put n
  put (CReqDelete f n)                      = putWord8 7  >> put f >> put n
  put (CReqUnknown)                         = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1  -> get >>= \f -> return (CReqContains f)
        2  -> get >>= \f -> return (CReqGetFileSites f)
        3  -> get >>= \f -> get >>= \s -> return (CReqGetNearestNodePortWithFile f s)
        4  -> get >>= \f -> get >>= \l -> get >>= \s -> return (CReqGetNearestNodePortForFile  f l s)
        5  -> get >>= \f -> get >>= \n -> return (CReqCreate f n)
        6  -> get >>= \f -> get >>= \n -> return (CReqAppend f n)
        7  -> get >>= \f -> get >>= \n -> return (CReqDelete f n)
        _  -> return (CReqUnknown)


-- | Response datatype from a filesystem Controller.
data ControllerResponseMessage
  = CRspSuccess
  | CRspGetFileSites (Set Site.SiteId)
  | CRspContains Bool
  | CRspGetNearestNodePortWithFile (Maybe ClientPort)
  | CRspGetNearestNodePortForFile (Maybe ClientPort)
  | CRspError String
  | CRspUnknown
  deriving (Show)
  
  
instance RspMsg ControllerResponseMessage where
  isError (CRspError _) = True
  isError _ = False
  
  getErrorMsg (CRspError e) = e
  getErrorMsg _ = ""
  
  isUnknown (CRspUnknown) = True
  isUnknown _ = False
  
  mkErrorMsg e = CRspError e


instance Binary ControllerResponseMessage where
  put (CRspSuccess)                       = putWord8 1
  put (CRspGetFileSites s)                = putWord8 2 >> put s 
  put (CRspContains b)                    = putWord8 3 >> put b
  put (CRspGetNearestNodePortWithFile p)  = putWord8 4 >> put p
  put (CRspGetNearestNodePortForFile p)   = putWord8 5 >> put p
  put (CRspError e)                       = putWord8 6 >> put e
  put (CRspUnknown)                       = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (CRspSuccess)
        2 -> get >>= \s -> return (CRspGetFileSites s) 
        3 -> get >>= \b -> return (CRspContains b)
        4 -> get >>= \p -> return (CRspGetNearestNodePortWithFile p)
        5 -> get >>= \p -> return (CRspGetNearestNodePortForFile p)
        6 -> get >>= \e -> return (CRspError e)
        _ -> return (CRspUnknown)



-- ----------------------------------------------------------------------------
-- Message Types from and to the Node
-- ----------------------------------------------------------------------------


-- | Requests datatype, which is send to a filesystem node.
data NodeRequestMessage 
  = NReqCreate          S.FileId S.FileContent
  | NReqAppend          S.FileId S.FileContent
  | NReqDelete          S.FileId Bool
  | NReqCopy            S.FileId ClientPort
  | NReqContains        S.FileId
  | NReqGetFileContent  S.FileId
  | NReqGetFileData     S.FileId
  | NReqGetFileIds      
  | NReqUnknown         
  deriving (Show)


instance Binary NodeRequestMessage where
  put (NReqCreate i c)       = putWord8 1 >> put i >> put c
  put (NReqAppend i c)       = putWord8 2 >> put i >> put c
  put (NReqDelete i b)       = putWord8 3 >> put i >> put b
  put (NReqCopy i cp)        = putWord8 4 >> put i >> put cp
  put (NReqContains i)       = putWord8 5 >> put i
  put (NReqGetFileContent i) = putWord8 6 >> put i
  put (NReqGetFileData i)    = putWord8 7 >> put i
  put (NReqGetFileIds)       = putWord8 8
  put (NReqUnknown)          = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> get >>= \i -> get >>= \c -> return (NReqCreate i c) 
        2 -> get >>= \i -> get >>= \c -> return (NReqAppend i c) 
        3 -> get >>= \i -> get >>= \b -> return (NReqDelete i b)
        4 -> get >>= \i -> get >>= \cp -> return (NReqCopy i cp)
        5 -> get >>= \i -> return (NReqContains i)
        6 -> get >>= \i -> return (NReqGetFileContent i)
        7 -> get >>= \i -> return (NReqGetFileData i)
        8 -> return (NReqGetFileIds)
        _ -> return (NReqUnknown)


-- | Response datatype from a filesystem node.
data NodeResponseMessage
  = NRspSuccess
  | NRspContains Bool
  | NRspGetFileContent (Maybe S.FileContent)
  | NRspGetFileData (Maybe S.FileData)
  | NRspGetFileIds [S.FileId]
  | NRspError String
  | NRspUnknown
  deriving (Show)      


instance RspMsg NodeResponseMessage where
  isError (NRspError _) = True
  isError _ = False
  
  getErrorMsg (NRspError e) = e
  getErrorMsg _ = ""
  
  isUnknown (NRspUnknown) = True
  isUnknown _ = False
  
  mkErrorMsg e = NRspError e

        
instance Binary NodeResponseMessage where
  put (NRspSuccess)          = putWord8 1
  put (NRspContains b)       = putWord8 2 >> put b
  put (NRspGetFileContent c) = putWord8 3 >> put c
  put (NRspGetFileData d)    = putWord8 4 >> put d
  put (NRspGetFileIds ls)    = putWord8 5 >> put ls
  put (NRspError e)          = putWord8 6 >> put e
  put (NRspUnknown)          = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return (NRspSuccess)
        2 -> get >>= \b  -> return (NRspContains b)
        3 -> get >>= \c  -> return (NRspGetFileContent c)
        4 -> get >>= \d  -> return (NRspGetFileData d)
        5 -> get >>= \ls -> return (NRspGetFileIds ls)
        6 -> get >>= \e  -> return (NRspError e)
        _ -> return (NRspUnknown)