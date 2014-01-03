{-# LANGUAGE DoAndIfThenElse #-}
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
module Client.Assets.FileSystem(
    FileSystemArchive
  ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either
import System.Directory
import System.FilePath
import Client.Assets.Archive
import System.Directory ()
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
  
newtype FileSystemArchive = FileSystemArchive FilePath

instance Archive FileSystemArchive where
  openArchive path = do 
    ex <- liftIO $ doesDirectoryExist path 
    if ex then do
      p <- liftIO $ getPermissions path
      if readable p && writable p
      then right $ FileSystemArchive path
      else left "Doesn't have permissions"
    else left "Directory isn't exists" 
    
  closeArchive _ = return ()
  
  listArchive (FileSystemArchive path) = getDirectoryContents path
  
  readArchiveFile (FileSystemArchive path) file = do
    res <- liftIO (try (readFile $ path </> file) :: IO (Either SomeException ByteString))
    eitherT (left.show) right $ hoistEither res
    
  writeArchiveFile (FileSystemArchive path) file ds = do
    res <- liftIO (try (writeFile (path </> file) ds) :: IO (Either SomeException ()))
    eitherT (left.show) right $ hoistEither res
      
  archivePath (FileSystemArchive path) = path