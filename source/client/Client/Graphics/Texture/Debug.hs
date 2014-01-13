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
  , textureQuad
  , screenQuad
  ) where
  
import Client.Graphics.GPipe
import Client.Graphics.Common
  
showTexture :: Texture2D RGBFormat -> FrameBuffer RGBFormat () ()
showTexture tex = paintSolid (textureQuad tex) emptyFrameBuffer

textureQuad :: Texture2D RGBFormat -> FragmentStream (Color RGBFormat (Fragment Float))
textureQuad tex = fmap texturise $ rasterizeFront transformedQuad
  where 
    texturise = sample (Sampler Linear Wrap) tex
  
-- | Some trivial transformations for viewport quad.      
transformedQuad :: PrimitiveStream Triangle (Vec4 (Vertex Float), Vec2 (Vertex Float))
transformedQuad = fmap homonize screenQuad
  where 
    homonize :: (Vec3 (Vertex Float), Vec2 (Vertex Float)) -> (Vec4 (Vertex Float), Vec2 (Vertex Float))
    homonize (v,uv) = (homPoint v :: Vec4 (Vertex Float), uv)  
        
screenQuad :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec2 (Vertex Float))
screenQuad = toGPUStream TriangleList $ zip [(-1):.(-1):.0:.(), 1:.1:.0:.(),     (-1):.1:.0:.(), 
                                             (-1):.(-1):.0:.(), 1:.(-1):.0:.(),  1:.1:.0:.()]
                                            [0:.1:.(), 1:.0:.(), 0:.0:.(), 0:.1:.(), 1:.1:.(), 1:.0:.()]