{-# LANGUAGE ExistentialQuantification #-}
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
module Client.Graphics.Texture.Debug(
    showTexture
  , combineTextures
  , textureQuad
  , screenQuad
  , SomeTexture(..)
  , blitTextures
  ) where
  
import Client.Graphics.GPipe
import Client.Graphics.Common
import Client.Graphics.GPipe.Inner.Formats (toColor, fromColor)
import Data.List

-- | Incapsulates textures with different color formats
data SomeTexture = forall f . (ColorFormat f) => SomeTexture (Texture2D f)

-- | Combines a number of textures into one.  
blitTextures :: Vec2 Int -- ^ Size of required final texture, all textures auto-resized 
  -> [(SomeTexture, Vec2 Float, Vec2 Float)] -- ^ list of textures and their region pos and size in resulting textures, coordinates
                                             -- are relative from 0 to 1.
  -> Texture2D RGBAFormat
blitTextures buffSize [] = fromFrameBufferColor RGBA8 buffSize emptyFrameBufferAlpha
blitTextures buffSize [(SomeTexture tex, origin, size)] = fromFrameBufferColor RGBA8 buffSize $ showTexture true tex origin size
blitTextures buffSize (tex:texs) = foldl' combine (blitTextures buffSize [tex]) texs
  where
    combine :: Texture2D RGBAFormat -> (SomeTexture, Vec2 Float, Vec2 Float) -> Texture2D RGBAFormat
    combine acc (SomeTexture blitTex, origin, size) = fromFrameBufferColor RGBA8 buffSize $ combineTextures acc blitTex origin size
 
showTexture :: (ColorFormat f) => Bool -> Texture2D f -> Vec2 Float -> Vec2 Float -> FrameBuffer RGBAFormat () ()
showTexture vflip tex orig size = paintSolidAlpha (textureQuad vflip tex orig size) emptyFrameBufferAlpha

combineTextures :: (ColorFormat f1, ColorFormat f2) => Texture2D f1 -> Texture2D f2
  -> Vec2 Float -> Vec2 Float -> FrameBuffer RGBAFormat () ()
combineTextures base tex origin size = paintSolidAlpha (textureQuad' base tex origin size) emptyFrameBufferAlpha

textureQuad :: (ColorFormat f) => Bool -> Texture2D f ->  Vec2 Float -> Vec2 Float -> FragmentStream (Color RGBAFormat (Fragment Float))
textureQuad vflip tex origin@(ox:.oy:.()) size@(sx:.sy:.()) = fmap texturise $ rasterizeFront transformedQuad
  where 
    texturise uv@(uvx:.uvy:.()) = if vflip 
      then let uv' = (uvx:.(1-uvy):.()) in texturise' uv' 
      else texturise' uv
    texturise' uv = ifB (isInside uv) (texColor uv) (toColor $ 0:.0:.0:.1:.())
    texColor = toColor . fromColor 0 1 . sample (Sampler Linear Wrap) tex  . remapCoords origin size
    isInside (uvx:.uvy:.()) = uvx >=* toGPU ox &&* uvy >=* toGPU oy &&* uvx <* toGPU (ox+sx) &&* uvy <* toGPU (oy+sy)
    
textureQuad' :: (ColorFormat f1, ColorFormat f2) => Texture2D f1 -> Texture2D f2
  -> Vec2 Float -> Vec2 Float
  -> FragmentStream (Color RGBAFormat (Fragment Float))
textureQuad' base tex origin@(ox:.oy:.()) size@(sx:.sy:.()) = fmap texturise $ rasterizeFront transformedQuad
  where
    texturise uv = ifB (isInside uv) (newColor uv) (baseColor uv)
    baseColor = toColor . fromColor 0 1 . sample (Sampler Linear Wrap) base
    newColor  = toColor . fromColor 0 1 . sample (Sampler Linear Wrap) tex  . remapCoords origin size
    isInside (uvx:.uvy:.()) = uvx >=* toGPU ox &&* uvy >=* toGPU oy &&* uvx <* toGPU (ox+sx) &&* uvy <* toGPU (oy+sy)  

remapCoords :: Vec2 Float -> Vec2 Float -> Vec2 (Fragment Float) -> Vec2 (Fragment Float)
remapCoords (ox:.oy:.()) (sx:.sy:.()) (ux:.uy:.()) = ux':.uy':.()
  where
    ux' = (ux - toGPU ox) / toGPU sx
    uy' = (uy - toGPU oy) / toGPU sy
     
-- | Some trivial transformations for viewport quad.      
transformedQuad :: PrimitiveStream Triangle (Vec4 (Vertex Float), Vec2 (Vertex Float))
transformedQuad = fmap homonize screenQuad
  where 
    homonize :: (Vec3 (Vertex Float), Vec2 (Vertex Float)) -> (Vec4 (Vertex Float), Vec2 (Vertex Float))
    homonize (v,uv) = (homPoint v :: Vec4 (Vertex Float), uv)  
        
screenQuad :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec2 (Vertex Float))
screenQuad = toGPUStream TriangleList $ zip vecs uvs                                         
  where
    vecs = [(-1):.(-1):.0:.(), 1:.1:.0:.(), (-1):.1:.0:.(), (-1):.(-1):.0:.(), 1:.(-1):.0:.(), 1:.1:.0:.()]  
    uvs = [0:.1:.(), 1:.0:.(), 0:.0:.(), 0:.1:.(), 1:.1:.(), 1:.0:.()]                                          