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
  , SomeTexture(..)
  , isAtlasModified
  , atlasTexture
  , emptyAtlas
  , atlasShape
  , updateAtlas
  , renderAtlas
  , removeFromAtlas
  , textureUvInAtlas
  ) where
  
import Prelude hiding (lookup)
import Client.Graphics.GPipe
import Client.Graphics.Texture.Render
import Client.Graphics.Texture.Repa
import Client.Assets.Manager
import Client.Assets.Texture
import Data.HashMap
import Data.List (foldl')
import Control.Monad.Trans.Either
import Data.Functor
import Control.Arrow (second, first)
import Control.Monad.IO.Class (liftIO)

-- | Texture atlas, assumes that subtextures has same size (will resize smaller ones to fit ceil)  
data Atlas = Atlas 
  -- | Modified flag, if true, then final texture should be rendered again
  Bool
  -- | Element size in pixels 
  (Vec2 Int)
  -- | Map from texture resource name to position in the atlas 
  LookupTable 
  -- | Final texture of the atlas
  (Texture2D RGBAFormat)

type LookupTable = Map String SubtexPlace
type SubtexPlace = Vec2 Int
type AtlasShape = Vec2 Int

-- | Checks modified flag of the atlas
isAtlasModified :: Atlas -> Bool
isAtlasModified (Atlas f _ _ _) = f

-- | Gets altas current texture
atlasTexture :: Atlas -> Texture2D RGBAFormat
atlasTexture (Atlas _ _ _ tex) = tex

-- | Returns empty atlas with empty texture inside
emptyAtlas :: Vec2 Int -- ^ Atlas element size in pixels
  -> Atlas
emptyAtlas elemSize = Atlas False elemSize empty (fromFrameBufferColor RGBA8 (0:.0:.()) (newFrameBufferColorDepth (RGBA 0 0) 100))

-- | Returns atlas shape, how subtextures will be layered into final texture
atlasShape :: Atlas -> AtlasShape
atlasShape (Atlas _ _ lookt _) = formShape $ size lookt 

-- | Calculates square shape from number of subtextures
formShape :: Int -> AtlasShape
formShape n = let side = ceiling (sqrt (fromIntegral n :: Double)) in side :. side :. ()

-- | Finds place for new texture in atlas filled with n textures and provided shape
toPlace :: Int -> AtlasShape -> SubtexPlace
toPlace n (shx:.shy:.()) = (n `mod` shx):.((n `div` shx) `mod` shy):.()
 
-- | Adds new textures to the atlas
updateAtlas :: Atlas -> [String] -> Atlas
updateAtlas (Atlas _ esize table tex) texs = Atlas True esize table'' tex
  where
    -- update old values
    table'  = snd $ mapAccum (\i _ -> (i+1, newPlace i) ) 0 table
    -- add new values
    table'' = snd $ foldl' (\(i, t) subtex -> (i+1, insert subtex (newPlace i) t)) (size table', table') texs'
    
    newPlace i = toPlace i (formShape newsize)
    texs' = Prelude.filter (\e -> not $ e `member` table) texs
    newsize = size table + length texs

-- | Removes textures from the atlas,
removeFromAtlas :: Atlas -> [String] -> Atlas
removeFromAtlas (Atlas _ esize table tex) texs = Atlas True esize table' tex
  where
    table' = snd $ foldWithKey (\subtex _ (i, t)-> if subtex `member` delTable then (i, t) else (i+1, insert subtex (newPlace i) t)) (0, empty) table
    delTable = fromList $ Prelude.map (,()) texs
    newPlace i = toPlace i (formShape newsize)
    newsize = size table - foldl' (\i subtex -> if subtex `member` table then i+1 else i) 0 texs

-- | Retrieves textures from resource manager and rerenders atlas
renderAtlas :: Atlas -> ResourceManager -> EitherT String IO (Atlas, ResourceManager)
renderAtlas oldAtlas@(Atlas modified esize table _) mng 
  | not modified = right (oldAtlas, mng)
  | otherwise    = do
    (texs, newmng) <- collect $ first loadTex <$> toList table
    atlasTex <- liftIO $ renderAtlasTexture esize texs (atlasShape oldAtlas)
    right (Atlas False esize table atlasTex, newmng)
  where
    loadTex :: String -> ResourceManager -> EitherT String IO (SomeTexture, ResourceManager)
    loadTex name mng' = first (\(TextureResource t) -> SomeTexture t) <$> getResource mng' name (Par2DRGBA RGBA8)
    
    collect :: [(ResourceManager -> EitherT String IO (SomeTexture, ResourceManager), SubtexPlace)] -> EitherT String IO ([(SomeTexture, SubtexPlace)], ResourceManager)
    collect = collect' mng []
      where
      collect' mng' _ [] = right ([], mng')
      collect' mng' acc [(action, place)] = do
        (tex, newmng) <- action mng'
        right ((tex, place) : acc, newmng) 
      collect' mng' acc ((action, place):es) = do
        (tex, mng'') <- action mng'
        collect' mng'' ((tex, place):acc) es

-- | Calculates atlas region for subtexture. Coordinates are relative from 0 to 1.
getPlaceRegion :: AtlasShape -> SubtexPlace -> (Vec2 Float, Vec2 Float)
getPlaceRegion (shx:.shy:.()) (ix:.iy:.()) = (ox:.oy:.(), sx:.sy:.())
  where
    sx = 1 / fromIntegral shx
    sy = 1 / fromIntegral shy
    ox = sx * fromIntegral ix
    oy = sy * fromIntegral iy

-- | Actually rerenders atlas inner texture
renderAtlasTexture :: Vec2 Int -> [(SomeTexture, SubtexPlace)] -> AtlasShape -> IO (Texture2D RGBAFormat)
renderAtlasTexture (esx:.esy:.()) texs shape@(shx:.shy:.()) = 
  cacheTexture atlasSize $ blitTextures atlasSize $ second (getPlaceRegion shape) <$> texs --  
  where
    atlasSize = (esx*shx):.(esy*shy):.()

-- | If texture is located in the atlas then returns texture region.    
textureUvInAtlas :: Atlas -> String -> Maybe (Vec2 Float, Vec2 Float)
textureUvInAtlas atlas@(Atlas _ _ table _) texname = do
  place <- texname `lookup` table
  return $ getPlaceRegion (atlasShape atlas) place