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
{-# LANGUAGE DeriveDataTypeable #-}
module Client.Assets.ResourcePack(
    ResourcePack()
  , resourcePackName
  , resourcePackPath
  , newResourcePack
  , getResource
  , setResource
  , finalizePack
  ) where
  
import Prelude hiding (lookup)  
import Data.HashMap
import Data.Dynamic
import Client.Assets.Resource
import Client.Assets.Archive
import Control.Monad.Trans.Either

data ResourcePack a = ResourcePack String a ResourceCache
  deriving (Typeable)
  
type ResourceCache = Map FilePath Dynamic 

resourcePackName :: ResourcePack a -> String
resourcePackName (ResourcePack name _ _) = name

resourcePackPath :: (Archive a) => ResourcePack a -> FilePath
resourcePackPath (ResourcePack _ archive _) = archivePath archive

newResourcePack :: (Archive a) => String -> a -> ResourcePack a
newResourcePack name archive = ResourcePack name archive empty

getResource :: (Archive a, Resource b) => ResourcePack a -> FilePath -> ResourceParams b -> EitherT String IO (b, ResourcePack a) 
getResource pack@(ResourcePack name archive cache) path params =
  case lookup path cache of
    Nothing -> newres
    Just cached -> 
      case fromDynamic cached of
        Nothing -> newres
        Just res -> right (res, pack)
  where
    newres = do
      res <- loadResource' path params =<< readArchiveFile archive path
      right (res, ResourcePack name archive (insert path (toDyn res) cache))

setResource :: (Archive a, Resource b) => ResourcePack a -> FilePath -> ResourceParams b -> b -> EitherT String IO (ResourcePack a)
setResource (ResourcePack name archive cache) path params res = do
  writeArchiveFile archive path =<< saveResource' path params res
  right $ ResourcePack name archive $ insert path (toDyn res) cache
  
finalizePack :: (Archive a) => ResourcePack a -> IO ()
finalizePack (ResourcePack _ archive _) = closeArchive archive  