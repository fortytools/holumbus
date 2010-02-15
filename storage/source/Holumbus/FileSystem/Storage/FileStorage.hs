-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.FileSystem.Storage.FileStorage
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  Implementation of the storage class.
  The FileStorage saves each file in a single physical file. The metadata of
  the files is hold in an extra directory an saved on disk.

-}

-- ----------------------------------------------------------------------------

module Holumbus.FileSystem.Storage.FileStorage
    (
      -- * datatypes
      FileStorage

      -- * storage initialisation
    , newFileStorage 
    )
where

import           Control.Monad
--import           Control.Exception
--import qualified Data.ByteString.Lazy as B
import           Data.Binary
--import           Holumbus.Common.MRBinary
import           Data.Maybe
import           System.IO
import           System.Directory
import           System.Log.Logger
import qualified Data.Map as Map

import           Holumbus.Common.Utils		 ( handleAll )
import           Holumbus.Common.FileHandling
import qualified Holumbus.FileSystem.Storage	 as S

localLogger :: String
localLogger = "Holumbus.FileSystem.Storage.FileStorage"

-- | The directory of the files in the storage. 
type StorageFileDirectory = Map.Map S.FileId S.FileData

-- | The filestorage datatype.
--   Every file is physically saved in one file on the harddisk
data FileStorage = MkFileStorage {
    fs_Path        :: FilePath             -- ^ path to the storage directory
  , fs_DirfilePath :: FilePath             -- ^ path to the directory file
  , fs_Directory   :: StorageFileDirectory -- ^ the directory with the metadata
  } deriving (Show)

-- -----------------------------------------------------------------------------
-- internal operations - adding and deleting files to the directory
-- -----------------------------------------------------------------------------

-- | Add or update a new file to the directory. 
addFileData :: StorageFileDirectory -> S.FileData -> StorageFileDirectory
addFileData dir new 
  = Map.insert fn dat dir
    where
      fn = (S.fd_FileId new)
      dat = maybe new (S.updateFileData new) (Map.lookup fn dir)

-- | Delete a new file to the directory.
deleteFileData :: StorageFileDirectory -> S.FileId -> StorageFileDirectory
deleteFileData dir i = Map.delete i dir

-- | Lookup the metadata from the directory.
lookupFileData :: StorageFileDirectory -> S.FileId -> (Maybe S.FileData)
lookupFileData dir i = Map.lookup i dir

-- | Get all fileIds from the directory
getIds :: StorageFileDirectory -> [S.FileId]
getIds dir = Map.keys dir

-- | Check, if a fileId is a member of the storage
isMember :: StorageFileDirectory -> S.FileId -> Bool
isMember dir i = Map.member i dir



-- -----------------------------------------------------------------------------
-- file storage operations and typeclass 
-- -----------------------------------------------------------------------------



-- | Create a new filestorage, which is empty an contains no files 
newFileStorage 
  :: FilePath           -- ^ the path of the filestorage on disk 
  -> FilePath           -- ^ the name of the directory file
  -> FileStorage
newFileStorage path name 
  = MkFileStorage path (path ++ name) Map.empty


-- | Loads the filestorage from disk      
readDirectory :: FileStorage -> IO (FileStorage)
readDirectory stor
  = do
    handleAll (\_ -> writeDirectory stor) $ do
      infoM localLogger ("opening filestorage directory: " ++ (fs_DirfilePath stor))
      file <- decodeFile (fs_DirfilePath stor)
      file `seq` return (stor {fs_Directory = file})

{-      bracket
        (do
         infoM localLogger ("opening filestorage directory: " ++ (fs_DirfilePath stor))
         openFile (fs_DirfilePath stor) ReadMode
        )
        (hClose)
        (\hdl -> 
          do
          pkg <- liftM words $ hGetLine hdl
          raw <- B.hGet hdl (read $ head pkg)
          dir <- return (decode raw)
          return (stor {fs_Directory = dir})
        )-}


-- | Saves the filestorage on disk  
writeDirectory :: FileStorage -> IO (FileStorage)
writeDirectory stor
  = do
    infoM localLogger ("writing filestorage directory: " ++ (fs_DirfilePath stor))
    createDirectoryIfMissing True (fs_Path stor)
    encodeFile (fs_DirfilePath stor) (fs_Directory stor)
    return stor

{-    bracket 
      (do
       infoM localLogger ("writing filestorage directory: " ++ (fs_DirfilePath stor))
       createDirectoryIfMissing True (fs_Path stor)
       debugM localLogger "directory created .. "
       openFile (fs_DirfilePath stor) WriteMode)
      (hClose) 
      (\hdl ->
        do 
        debugM localLogger "encode store.. "
        enc <- return (encode $ fs_Directory stor)

        debugM localLogger "calc length .. "
        hPutStrLn hdl ((show $ B.length enc) ++ " ")

        debugM localLogger "put store .. "
        B.hPut hdl enc    

        debugM localLogger "return .. "
        return stor
      )-}

writeSingleFile :: FileStorage -> S.FileId -> S.FileContent -> IO FileStorage
writeSingleFile stor fn c = do
  debugM localLogger "write to bin file .. "
  writeToBinFile path c
  debugM localLogger "create file metadata"
  dat <- S.createFileData fn c
  return $ stor {fs_Directory = newdir dat}  
  where
    path = (fs_Path stor) ++ fn
    newdir d = addFileData (fs_Directory stor) d


-- | deriving FileStorage from the class Storage
instance S.Storage FileStorage where

  openStorage = readDirectory


  closeStorage = writeDirectory


  createFile stor fn c 
    = do
      stor' <- writeSingleFile stor fn c
      debugM localLogger "write directory"
      writeDirectory stor'

  createFiles stor l
    = do
      debugM localLogger "write to bin file .. "
      stor' <- writeFiles stor l
      debugM localLogger "write directory"
      writeDirectory stor'
      where
        writeFiles :: FileStorage -> [(S.FileId,S.FileContent)] -> IO FileStorage
        writeFiles stor [] = return stor
        writeFiles stor ((fn,c):xs) = do
          stor' <- writeSingleFile stor fn c
          writeFiles stor' xs
 

  deleteFile stor i
    = do
      stor' <- 
        if isMember (fs_Directory stor) i
          then do 
            removeFile path
            return (stor {fs_Directory = newdir})
          else
            return stor
      writeDirectory $ stor'
      where
        path = (fs_Path stor) ++ i
        newdir = deleteFileData (fs_Directory stor) i


  appendFile stor i c
    = do
      if isMember (fs_Directory stor) i
        then do
          appendToBinFile path c
              -- case c of
              --  (S.TextFile t) -> appendToTextFile path t
              --  (S.ListFile l) -> appendToListFile path l
              --  (S.BinFile  b) -> appendToBinFile path b
          dat <- S.createFileData i c
          writeDirectory $ stor { fs_Directory = newdir dat }
        else do
          S.createFile stor i c      
      where
        path = (fs_Path stor) ++ i
        newdir d = addFileData (fs_Directory stor) d


  containsFile stor i
    = do
      return (isMember (fs_Directory stor) i)

  
  getFileContent stor i
    = do
      infoM localLogger $ "getFileContent: reading " ++ show i
      if (isMember (fs_Directory stor) i) 
        then do
          handleAll (\e -> do
              errorM localLogger $ "getFileContent: " ++ show e
              return Nothing
            ) $ 
            do
            -- metaData <- S.getFileData stor i
            c <- readFromBinFile path
            {-
            c <- case (S.fd_Type $ fromJust metaData) of
              (S.FTText) -> 
                 do
                 t <- readFromTextFile path
                 return $ S.TextFile t
              (S.FTList) ->
                 do
                 l <- readFromListFile path
                 return $ S.ListFile l
              (S.FTBin)  -> 
                 do
                 b <- readFromBinFile path
                 return $ S.BinFile b
            -}
            debugM localLogger $ "getFileContent: content: " ++ show c
            infoM localLogger $ "getFileContent: finished "
            return (Just c)
        else do return Nothing
      where
        path = (fs_Path stor) ++ i


  getFileData stor i
    = do
      return (lookupFileData (fs_Directory stor) i)


  getFileIds stor
    = do
      return (getIds (fs_Directory stor))
