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
import           Control.Exception
import qualified Data.ByteString.Lazy as B
import           Data.Binary
import           System.IO
import           System.Directory
import           System.Log.Logger
import qualified Data.Map as Map

import qualified Holumbus.FileSystem.Storage as S

localLogger :: String
localLogger = "Holumbus.FileSystem.Storage.FileStorage"

-- | The directory of the files in the storage. 
type StorageFileDirectory = Map.Map S.FileId S.FileData

-- | The filestorage datatype.
--   Every file is physically saved in one file on the harddisk
data FileStorage = MkFileStorage {
    fs_path        :: FilePath             -- ^ path to the storage directory
  , fs_dirfilepath :: FilePath             -- ^ path to the directory file
  , fs_directory   :: StorageFileDirectory -- ^ the directory with the metadata
  } deriving (Show)

-- -----------------------------------------------------------------------------
-- internal operations - adding and deleting files to the directory
-- -----------------------------------------------------------------------------

-- | Add a new file to the directory. 
addFileData :: StorageFileDirectory -> S.FileData -> StorageFileDirectory
addFileData dir new 
  = Map.insert fn dat dir
    where
      fn = (S.fd_fileId new)
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


defaultDirectoryFileName :: FilePath
defaultDirectoryFileName = "directory"


-- | Create a new filestorage, which is empty an contains no files 
newFileStorage 
  :: FilePath           -- ^ the path of the filestorage on disk 
  -> (Maybe FilePath)   -- ^ the name of the directory file, if "Nothing" the default name will be used
  -> FileStorage
newFileStorage path name 
  = MkFileStorage path (path ++ dirName) Map.empty
  where
    dirName = maybe defaultDirectoryFileName id name


-- | Loads the filestorage from disk      
readDirectory :: FileStorage -> IO (FileStorage)
readDirectory stor
  = do
    handle (\_ -> writeDirectory stor) $
      bracket
        (do
         debugM localLogger ("opening filestorage directory: " ++ (fs_dirfilepath stor))
         openFile (fs_dirfilepath stor) ReadMode
        )
        (hClose)
        (\hdl -> 
          do
          pkg <- liftM words $ hGetLine hdl
          raw <- B.hGet hdl (read $ head pkg)
          dir <- return (decode raw)
          return (stor {fs_directory = dir})
        )


-- | Saves the filestorage on disk  
writeDirectory :: FileStorage -> IO (FileStorage)
writeDirectory stor
  = do
    bracket 
      (openFile (fs_dirfilepath stor) WriteMode)
      (hClose) 
      (\hdl ->
        do 
        enc <- return (encode $ fs_directory stor)
        hPutStrLn hdl ((show $ B.length enc) ++ " ")
        B.hPut hdl enc    
        return stor
      )


-- | deriving FileStorage from the class Storage
instance S.Storage FileStorage where

  openStorage = readDirectory


  closeStorage = writeDirectory


  createFile stor fn c 
    = do
      writeToBinFile path c
      -- update the directory   
      dat <- S.createFileData fn c
      writeDirectory $ stor {fs_directory = newdir dat} 
      {-bracket 
        (openFile path WriteMode)
        (hClose)
        (\hdl ->
          do
          -- write new file
          enc <- return (encode $ c)
          hPutStrLn hdl ((show $ B.length enc) ++ " ")
          B.hPut hdl enc
          
          -- update the directory   
          dat <- S.createFileData fn c
          writeDirectory $ stor {fs_directory = newdir dat} 
        )-}
      where
        path = (fs_path stor) ++ fn
        newdir d = addFileData (fs_directory stor) d 


  deleteFile stor i
    = do
      stor' <- 
        if isMember (fs_directory stor) i
          then do 
            removeFile path
            return (stor {fs_directory = newdir})
          else
            return stor
      writeDirectory $ stor'
      where
        path = (fs_path stor) ++ i
        newdir = deleteFileData (fs_directory stor) i


  appendFile stor i c
    = do
      if isMember (fs_directory stor) i
        then do
          appendToBinFile path c
          -- update directory 
          dat <- S.createFileData i c
          writeDirectory $ stor { fs_directory = newdir dat }
          {-
          bracket
            (openFile path AppendMode)
            (hClose)
            (\hdl ->
              do
              -- TODO append new content
              -- write new file
              enc <- return (encode $ c)
              hPutStrLn hdl ((show $ B.length enc) ++ " ")
              B.hPut hdl enc   
              
              -- update directory 
              dat <- S.createFileData i c
              writeDirectory $ stor { fs_directory = newdir dat }
            )-}
        else do
          S.createFile stor i c      
      where
        path = (fs_path stor) ++ i
        newdir d = addFileData (fs_directory stor) d


  containsFile stor i
    = do
      return (isMember (fs_directory stor) i)

  --getFileContent :: s -> FileId -> IO (Maybe FileContent)
  getFileContent stor i
    = do
      debugM localLogger $ "getFileContent: reading " ++ show i
      if (isMember (fs_directory stor) i) 
        then do
          handle (\e -> do
              errorM localLogger $ "getFileContent: " ++ show e
              return Nothing
            ) $ 
            do
            c <- strictReadFileFile path
            debugM localLogger $ "getFileContent: content: " ++ show c
            return (Just c)
            {-bracket
              (do 
               debugM localLogger ("opening file - path: " ++ path)
               openFile path ReadMode
              )
              (hClose)
              (\hdl ->
                do
                -- read file
                pkg <- liftM words $ hGetLine hdl
                raw <- B.hGet hdl (read $ head pkg)
                let p = (decode raw)
                debugM localLogger ("file content: " ++ show p)
                return (Just p)
              )-}
        else do return Nothing
      where
        path = (fs_path stor) ++ i


  getFileData stor i
    = do
      return (lookupFileData (fs_directory stor) i)

  getFileIds stor
    = do
      return (getIds (fs_directory stor))


-- ----------------------------------------------------------------------------
-- Binary file Handling
-- ----------------------------------------------------------------------------

-- loadFromBinFile :: Binary a => FilePath -> IO a
-- loadFromBinFile f = decodeFile f

writeToBinFile :: Binary a => FilePath -> a -> IO ()
writeToBinFile = encodeFile

appendToBinFile :: Binary a => FilePath -> a -> IO ()
appendToBinFile f = B.appendFile f . encode


-- found on the haskell cafe mailing list
-- http:\/\/www.haskell.org\/pipermail\/haskell-cafe\/2008-April\/041970.html
strictReadFileFile :: Binary a => FilePath -> IO a
strictReadFileFile f  
   = bracket (openBinaryFile f ReadMode) hClose $ 
       \h -> do
       c <- B.hGetContents h
       return $! decode c
  

{-
  getFilePath stor fp
    = maybe Nothing calcPath (getFileData stor fp) 
    where
      calcPath dat = Just ((fs_path stor) ++ (fd_filename dat))
-}
  
  