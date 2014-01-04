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
{-# LANGUAGE ExistentialQuantification #-}
module Client.Assets.Manager(
    ResourceManager()
  , emptyResourceManager
  , addNewFileSystemPack
  ) where
  
import Prelude hiding (lookup)
import Data.HashMap
import Client.Assets.ResourcePack
import Client.Assets.Archive
import Client.Assets.FileSystem
import System.Log.Logger
import Data.Functor ((<$>))
import Control.Monad.Trans.Either (eitherT)

type ResourceManager = Map String SomePack

data SomePack = forall a . (Archive a) => SomePack (ResourcePack a)

emptyResourceManager :: ResourceManager
emptyResourceManager = empty

logg :: String -> IO ()
logg = warningM "GoreAndAsh.ResourceManager"

addNewFileSystemPack :: ResourceManager -> String -> FilePath -> IO ResourceManager
addNewFileSystemPack mng name path = do
  case name `lookup` mng of
    Just (SomePack oldPack) -> finalizePack oldPack 
    Nothing -> return ()
  eitherT (\msg -> logg msg >> return mng) (\pack -> return $ insert name (SomePack pack) mng) $ newResourcePack name <$> newFileSystemArchive path