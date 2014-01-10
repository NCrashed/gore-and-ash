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
  , getResource
  ) where
  
import Prelude hiding (lookup)
import Data.HashMap
import qualified Client.Assets.ResourcePack as Pack 
import Client.Assets.Archive
import Client.Assets.FileSystem
import Client.Assets.Resource
import System.Log.Logger
import Data.Functor
import Control.Monad.Trans.Either

type ResourceManager = Map String SomePack

data SomePack = forall a . (Archive a) => SomePack (Pack.ResourcePack a)

emptyResourceManager :: ResourceManager
emptyResourceManager = empty

logg :: String -> IO ()
logg = warningM "GoreAndAsh.ResourceManager"

getResourcePack :: ResourceManager -> String -> Maybe SomePack
getResourcePack mng name = name `lookup` mng

setResourcePack :: ResourceManager -> String -> SomePack -> ResourceManager
setResourcePack mng name pack = insert name pack mng

addNewFileSystemPack :: ResourceManager -> String -> FilePath -> IO ResourceManager
addNewFileSystemPack mng name path = do
  case getResourcePack mng name of
    Just (SomePack oldPack) -> Pack.finalizePack oldPack 
    Nothing -> return ()
  eitherT (\msg -> logg msg >> return mng) (\pack -> return $ insert name (SomePack pack) mng) $ Pack.newResourcePack name <$> newFileSystemArchive path
  
getResource :: (Resource a) => ResourceManager -> String -> ResourceParams a -> EitherT String IO (a, ResourceManager)
getResource mng fullName params = case getResourcePack mng packName of
  Nothing -> left $ "Cannot find resource pack with name \"" ++ packName ++ "\"!"
  Just (SomePack pack) -> do
    (res, newpack) <- Pack.getResource pack resName params
    return (res, setResourcePack mng packName $ SomePack newpack)
  where
    (packName, _:resName) = break (== ':') fullName