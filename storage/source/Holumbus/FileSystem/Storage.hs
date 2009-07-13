-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.FileSystem.Storage
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This Module contains the typeclass of a storage and the associated datatypes.
  
-}

-- ----------------------------------------------------------------------------

module Holumbus.FileSystem.Storage
(
-- * file datatypes
  FileId
, FileContent
--, getFileContentType

-- * file operation
, getContentLength

-- * metadata
, FileData(..)
, createFileData
, updateFileData

-- * storage typeclass
, Storage(..)
)
where

import           Prelude hiding (appendFile)
--import           Data.Binary
import Holumbus.Common.MRBinary
import           Data.Time
import qualified Data.ByteString.Lazy as B


-- -----------------------------------------------------------------------------
-- the file content
-- -----------------------------------------------------------------------------

-- | The file-identifier, should be unique in the whole system and
--   be an instance of the classes show, eq, ord and binary
type FileId = String

{-
data FileType = FTText | FTList | FTBin
  deriving (Show, Eq, Ord, Enum)
  
instance Binary FileType where
  put (FTText) = putWord8 1
  put (FTList) = putWord8 2
  put (FTBin) = putWord8 0
  get
    = do
      t <- getWord8
      case t of
        1 -> return FTText
        2 -> return FTList
        _ -> return FTBin
-}

type FileContent = B.ByteString

{-
-- | The content of a file, this will be generic in further versions
data FileContent 
  = TextFile String
  | ListFile [B.ByteString]
  | BinFile B.ByteString
  deriving (Show)


-- | gets the type of the file content
getFileContentType :: FileContent -> FileType
getFileContentType (TextFile _) = FTText
getFileContentType (ListFile _) = FTList
getFileContentType (BinFile _)  = FTBin
-}

getContentLength :: FileContent -> Integer
getContentLength c = fromIntegral $ B.length c

{-
-- | The length of the file-content
getContentLength :: FileContent -> Integer
getContentLength (TextFile s) = fromIntegral $ length s
getContentLength (ListFile s) = fromIntegral $ length s
getContentLength (BinFile s)  = fromIntegral $ B.length s
-}


-- | A hash function for the content, to compare two files
getContentHash :: FileContent -> Integer
getContentHash _ = 0


{-
instance Binary FileContent where
  put (TextFile s) = putWord8 1 >> put s
  put (ListFile s) = putWord8 2 >> put s
  put (BinFile s)  = putWord8 0 >> put s
  get 
    = do
      t <- getWord8
      case t of
        1 -> get >>= \s -> return (TextFile s)
        2 -> get >>= \s -> return (ListFile s)
        _ -> get >>= \s -> return (BinFile s)       
-}


-- -----------------------------------------------------------------------------
-- the file metadata
-- -----------------------------------------------------------------------------

-- | metadata of a file, known by the storage.
data FileData = MkFileData {
  --  fd_Type             :: FileType  -- ^ filetype
    fd_FileId           :: FileId    -- ^ filename
  , fd_Size             :: Integer   -- ^ filesize
  , fd_CreationDate     :: UTCTime   -- ^ creation date
  , fd_LastModifiedDate :: UTCTime   -- ^ last modified date
  , fd_Hashvalue        :: Integer   -- ^ hash value
  } deriving (Show)

-- | Create a new file data item.
createFileData :: FileId -> FileContent -> IO (FileData)
createFileData i c
  = do 
    time <- getCurrentTime
    return (MkFileData
      -- (getFileContentType c)
      i
      (getContentLength c)
      time
      time
      (getContentHash c)
     )

-- | Updates a new file data item with the date, size and hash value of
--   an old one. First parameter is the new item, second the old one.
updateFileData :: FileData -> FileData -> FileData
updateFileData new old
  = old {
      fd_Size             = fd_Size new
    , fd_LastModifiedDate = fd_LastModifiedDate new
    , fd_Hashvalue        = fd_Hashvalue new
    }


instance Binary FileData where
  put d 
    = -- put (fd_Type d) >>
      put (fd_FileId d) >> 
      put (fd_Size d) >> 
      put (show $ fd_CreationDate d) >> 
      put (show $ fd_LastModifiedDate d) >>
      put (fd_Hashvalue d)
  get 
    = do
      -- t    <- get
      name <- get
      size <- get
      dat1 <- get
      dat2 <- get
      hash <- get
      return (MkFileData name size (read dat1) (read dat2) hash) 



-- -----------------------------------------------------------------------------
-- the storage typeclass
-- -----------------------------------------------------------------------------

-- | The interface of a storage.  
class Storage s where

  -- | Initialize the storage.
  openStorage :: s -> IO (s)
  
  -- | Deinitialize the storage.
  closeStorage :: s -> IO (s)
  
  -- | Create a new file in the storage.
  --   Overwrite the file if it already exists.
  createFile :: s -> FileId -> FileContent -> IO (s)

  -- | Delete a file in the storage.
  --   Nothing happens if the file doesn't exist
  deleteFile :: s -> FileId -> IO (s)

  -- | Append the content of the file.
  --   Create a new file, if it doesn't already exist
  appendFile :: s -> FileId -> FileContent -> IO (s)

  -- | Check if the file is already in the storage
  containsFile :: s -> FileId -> IO Bool

  -- | Get the content of a file.
  --   Return Nothing, if the file doesn't exist
  getFileContent :: s -> FileId -> IO (Maybe FileContent)

  -- | Get the metadata of a file.
  --   Return Nothing, if the file doesn't exist
  getFileData :: s -> FileId -> IO(Maybe FileData)

  -- | List the files in the storage   
  getFileIds :: s -> IO [FileId]
