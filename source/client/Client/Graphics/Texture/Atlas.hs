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
{-# LANGUAGE ExistentialQuantification, TupleSections #-}
module Client.Graphics.Texture.Atlas(
    Atlas()
  , isAtlasModified
  , emptyAtlas
  , atlasShape
  , updateAtlas
  , renderAtlas
  , removeFromAtlas
  ) where
  
import Client.Graphics.GPipe
import Data.HashMap
import Data.List (foldl')
import Client.Assets.Manager
import Control.Monad.Trans.Either

-- | Texture atlas, assumes that subtextures has same size (will resize smaller ones to fit ceil)  
data Atlas = forall f . (ColorFormat f) => Atlas 
  -- | Modified flag, if true, then final texture should be rendered again
  Bool
  -- | Map from texture resource name to position in the atlas 
  LookupTable 
  -- | Final texture of the atlas
  (Texture2D f)

type LookupTable = Map String SubtexPlace
type SubtexPlace = Vec2 Int
type AtlasShape = Vec2 Int

-- | Checks modified flag of the atlas
isAtlasModified :: Atlas -> Bool
isAtlasModified (Atlas f _ _) = f

-- | Returns empty atlas with empty texture inside
emptyAtlas :: Atlas
emptyAtlas = Atlas False empty (fromFrameBufferColor RGBA8 (0:.0:.()) (newFrameBufferColorDepth (RGBA 0 0) 100))

-- | Returns atlas shape, how subtextures will be layered into final texture
atlasShape :: Atlas -> AtlasShape
atlasShape (Atlas _ lookt _) = formShape $ size lookt 

-- | Calculates square shape from number of subtextures
formShape :: Int -> AtlasShape
formShape n = let side = ceiling $ logBase 2 (fromIntegral n :: Double) in side :. side :. () 

-- | Finds place for new texture in atlas filled with n textures and provided shape
findPlace :: Int -> AtlasShape -> SubtexPlace
findPlace n (shx:.shy:.()) = (n `mod` shx):.((n `div` shx) `mod` shy):.()

-- | Adds new textures to the atlas
updateAtlas :: Atlas -> [String] -> Atlas
updateAtlas (Atlas _ table tex) texs = Atlas True table'' tex
  where
    -- update old values
    table'  = snd $ mapAccum (\i _ -> (i+1, newPlace i) ) 0 table
    -- add new values
    table'' = snd $ foldl' (\(i, t) subtex -> (i+1, insert subtex (newPlace i) t)) (size table', table') texs'
    
    newPlace i = findPlace i (formShape newsize)
    texs' = Prelude.filter (\e -> not $ e `member` table) texs
    newsize = size table + length texs

-- | Removes textures from the atlas,
removeFromAtlas :: Atlas -> [String] -> Atlas
removeFromAtlas (Atlas _ table tex) texs = Atlas True table' tex
  where
    table' = snd $ foldWithKey (\subtex _ (i, t)-> if subtex `member` delTable then (i, t) else (i+1, insert subtex (newPlace i) t)) (0, empty) table
    delTable = fromList $ Prelude.map (,()) texs
    newPlace i = findPlace i (formShape newsize)
    newsize = size table - foldl' (\i subtex -> if subtex `member` table then i+1 else i) 0 texs

-- | Actually rerenders atlas inner final texture
renderAtlas :: Atlas -> ResourceManager -> EitherT String IO Atlas
renderAtlas = undefined

-- | TODO: function to query atlas for subtexture uvs

-- | TODO: need testing for functions

-- | TODO: also instance for resource for atlas