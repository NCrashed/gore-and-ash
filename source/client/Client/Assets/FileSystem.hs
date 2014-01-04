-- Copyright 2013 Anton Gushcha
--    This file is part of Gore&Ash.
--
--    Gore&Ash is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Gore&Ash is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Gore&Ash.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE DoAndIfThenElse, DeriveDataTypeable #-}
module Client.Assets.FileSystem(
    FileSystemArchive
  , newFileSystemArchive
  ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either
import Data.Typeable
import Data.ByteString.Lazy (readFile, writeFile)
import System.Directory
import System.FilePath
import Client.Assets.Archive
import Util.Monad (liftExceptions)

newtype FileSystemArchive = FileSystemArchive FilePath
  deriving (Typeable)
  
instance Archive FileSystemArchive where
  openArchive path = do 
    ex <- liftIO $ doesDirectoryExist path 
    if ex then do
      p <- liftIO $ getPermissions path
      if readable p && writable p
      then right $ FileSystemArchive path
      else left "Doesn't have permissions"
    else left "Directory doesn't exist" 
    
  closeArchive _ = return ()
  
  listArchive (FileSystemArchive path) = getDirectoryContents path
  readArchiveFile (FileSystemArchive path) file = liftExceptions $ liftIO $ readFile $ path </> file
  writeArchiveFile (FileSystemArchive path) file ds = liftExceptions $ liftIO $ writeFile (path </> file) ds
  archivePath (FileSystemArchive path) = path
  
newFileSystemArchive :: FilePath -> EitherT String IO FileSystemArchive
newFileSystemArchive path = do
  liftExceptions $ liftIO $ createDirectoryIfMissing True path
  openArchive path