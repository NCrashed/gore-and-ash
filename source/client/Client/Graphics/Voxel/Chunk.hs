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
import Data.Vec
import Data.Maybe
import Client.Graphics.Common 
import Client.Graphics.Camera
import Client.Graphics.Raycasting.Ray
import Game.Boxed.Chunk


chunkFrameBuffer :: BoxedChunk -> Float -> Vec2 Int -> FrameBuffer RGBFormat DepthFormat ()
chunkFrameBuffer chunk angle size = paintSolid (rasterizedChunk chunk angle size) emptyFrameBuffer

rasterizedChunk :: BoxedChunk -> Float -> Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float), FragmentDepth)
rasterizedChunk chunk angle size@(width:.height:.()) = fmap (rayCast projViewInv chunk size) $ rasterizeFront transformedQuad
    where
        projMatrix = perspective 1 100 (pi/3) (fromIntegral width / fromIntegral height)
        viewMatrix = cameraMatrix $ newCamera (0:.0:.0:.()) (0:.0:.1:.()) (0:.1:.0:.())
        projViewMatrix = projMatrix `multmm` viewMatrix
        projViewInv = toGPU $ fromMaybe identity (invert projViewMatrix)

rayCast :: Mat44 (Fragment Float) -> BoxedChunk -> Vec2 Int -> () -> (Color RGBFormat (Fragment Float), FragmentDepth)
rayCast projViewInv chunk (width:.height:.()) _ = rayPixel chunk $ GPURay vecStart direction
    where
        zero :: Fragment Float
        zero = 0
        one :: Fragment Float
        one = 1
        viewX = (2.0 * (fragX + 0.5)) / fromIntegral width - 1.0
        viewY = (2.0 * (fragY + 0.5)) / fromIntegral height - 1.0
        vecStart = wtrans $ projViewInv `multmv` (viewX:.viewY:.zero:.one:.()) 
        vecEnd   = wtrans $ projViewInv `multmv` (viewX:.viewY:.one :.one:.()) 
        wtrans (x:.y:.z:.w:.()) = let iw = one / w in (x*iw):.(y*iw):.(z*iw):.()
        direction = normalize $ vecEnd - vecStart

rayPixel :: BoxedChunk -> GPURay (Fragment Float) -> (Color RGBFormat (Fragment Float), FragmentDepth)
rayPixel chunk (GPURay origin direction) = (RGB direction, 0)

transformedQuad :: PrimitiveStream Triangle (Vec4 (Vertex Float), ())
transformedQuad = fmap homonize screenQuad
    where 
        homonize :: Vec3 (Vertex Float) -> (Vec4 (Vertex Float), ())
        homonize v = (homPoint v :: Vec4 (Vertex Float), ())  

screenQuad :: PrimitiveStream Triangle (Vec3 (Vertex Float))
screenQuad = toGPUStream TriangleList [(-1):.(-1):.0:.(), 1:.1:.0:.(),     (-1):.1:.0:.(), 
                                       (-1):.(-1):.0:.(), 1:.(-1):.0:.(),  1:.1:.0:.()]