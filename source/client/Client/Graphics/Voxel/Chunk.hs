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
{-# LANGUAGE TupleSections #-}
module Client.Graphics.Voxel.Chunk(
      chunkFrameBuffer
    ) where
    
import Graphics.GPipe
import Client.Graphics.Common
import Game.Boxed.Chunk

chunkFrameBuffer :: BoxedChunk -> Float -> Vec2 Int -> FrameBuffer RGBFormat DepthFormat ()
chunkFrameBuffer chunk angle size = paintSolid (rasterizedChunk chunk angle size) emptyFrameBuffer

rasterizedChunk :: BoxedChunk -> Float -> Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float), FragmentDepth)
rasterizedChunk chunk angle size = fmap (testColor . const fragDepth) $ rasterizeFront $ transformedQuad

testColor :: FragmentDepth -> (Color RGBFormat (Fragment Float), FragmentDepth)
testColor depth = (RGB $ toGPU (0:.0.45:.1:.()), 0)

transformedQuad :: PrimitiveStream Triangle (Vec4 (Vertex Float), ())
transformedQuad = fmap homonize screenQuad
    where 
        homonize :: Vec3 (Vertex Float) -> (Vec4 (Vertex Float), ())
        homonize vec = (homPoint vec :: Vec4 (Vertex Float), ())  

screenQuad :: PrimitiveStream Triangle (Vec3 (Vertex Float))
screenQuad = toGPUStream TriangleList $  [(-1):.(-1):.0:.(), 1:.1:.0:.(),     (-1):.1:.0:.(), 
                                          (-1):.(-1):.0:.(), 1:.(-1):.0:.(),  1:.1:.0:.()]