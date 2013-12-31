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
    , convertChunk
    ) where
    
import Client.Graphics.GPipe
import Data.Vec as Vec
import Data.Maybe
import Client.Graphics.Common 
import Client.Graphics.Camera
import Client.Graphics.Raycasting.Ray
import Client.Graphics.Raycasting.Box
import Game.Boxed.Chunk

import Foreign.ForeignPtr.Safe (withForeignPtr)

newtype GPUChunk = GPUChunk (Texture3D RGBAFormat)

-- | Converts CPU chunk into GPU chunk to use in fragment shader.
convertChunk :: BoxedChunk -> IO GPUChunk
convertChunk chunk = do
    ptr <- getRawData chunk 
    tex <- withForeignPtr ptr $ \rawPtr -> newTexture UnsignedInt8_8_8_8 RGBA8 (chunkSizeVec chunk) [rawPtr]
    return $ GPUChunk tex

-- | Gets color from gpu chunk. Position coordinates should be between 0 and 1.
extractColor :: GPUChunk -> Vec3 (Fragment Float) -> Color RGBAFormat (Fragment Float)
extractColor (GPUChunk texture) = sample (Sampler Point Clamp) texture

-- | Draws boxed chunk into frame buffer to display. The last step of graphic pipe that
-- | paints resulted fragments into framebuffer.
chunkFrameBuffer :: GPUChunk -> Float -> Vec2 Int -> FrameBuffer RGBFormat DepthFormat ()
chunkFrameBuffer chunk angle size = paintSolid (rasterizedChunk chunk angle size) emptyFrameBuffer

-- | Rasterizes transformed screen quad and ray-casts chunk from each fragment. The main purpose to
-- | extract view-projection matrix and it inverse form to pass into raycasing function.
rasterizedChunk :: GPUChunk -> Float -> Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float), FragmentDepth)
rasterizedChunk chunk angle size@(width:.height:.()) = fmap (rayCast projViewInv chunk size) $ rasterizeFront transformedQuad
    where
        projMatrix = perspective 1 100 (pi/3) (fromIntegral width / fromIntegral height)
        viewMatrix = cameraMatrix $ newCamera rotatedVec rotatedVec (0:.1:.0:.())
        projViewMatrix = projMatrix `multmm` viewMatrix
        projViewInv = toGPU $ fromMaybe identity (invert projViewMatrix)
        rotatedVec = wtrans $ rotationVec (normalize (0:.1:.0:.())) angle `multmv` (0:.0:.(-5):.1:.())
        wtrans (x:.y:.z:.w:.()) = let iw = 1 / w in (x*iw):.(y*iw):.(z*iw):.()

-- | Calculates color for a ray that started from screen pixel. The main purpose is to
-- | define world-space ray origins and directions.       
rayCast :: Mat44 (Fragment Float) -> GPUChunk -> Vec2 Int -> () -> (Color RGBFormat (Fragment Float), FragmentDepth)
rayCast projViewInv chunk (width:.height:.()) _ = rayPixel chunk testBox $ GPURay vecStart direction
    where
        viewX = (2.0 * (fragX + 0.5)) / fromIntegral width - 1.0
        viewY = (2.0 * (fragY + 0.5)) / fromIntegral height - 1.0
        vecStart = wtrans $ projViewInv `multmv` (viewX:.viewY:.0:.1:.()) 
        vecEnd   = wtrans $ projViewInv `multmv` (viewX:.viewY:.1:.1:.()) 
        wtrans (x:.y:.z:.w:.()) = let iw = 1 / w in (x*iw):.(y*iw):.(z*iw):.()
        direction = normalize $ vecEnd - vecStart
        testBox = toGPU $ GPUBox ((-1):.(-1):.(-1):.()) (1:. 1:. 1:. ()) 
        
-- | Calculates color for a ray with known world-space origin and direction.        
rayPixel :: GPUChunk -> GPUBox (Fragment Float) -> GPURay (Fragment Float) -> (Color RGBFormat (Fragment Float), FragmentDepth)
rayPixel chunk box ray = (RGB color, 0)
    where
      (res, tmin, tmax) = intersectBoxAndRay box ray
      color = ifB (res &&* tmax >* 0) chunkColor (0 :. 0 :. 0 :. ())
      RGBA chunkColor _ = extractColor chunk $ toLocalBoxPos box $ rayPoint ray tmin

-- | Some trivial transformations for viewport quad.      
transformedQuad :: PrimitiveStream Triangle (Vec4 (Vertex Float), ())
transformedQuad = fmap homonize screenQuad
    where 
        homonize :: Vec3 (Vertex Float) -> (Vec4 (Vertex Float), ())
        homonize v = (homPoint v :: Vec4 (Vertex Float), ())  

-- | To start raycasting we need a fragment for each viewport pixel and to
-- | achive this a huge quad covering all viewport is produced.
screenQuad :: PrimitiveStream Triangle (Vec3 (Vertex Float))
screenQuad = toGPUStream TriangleList [(-1):.(-1):.0:.(), 1:.1:.0:.(),     (-1):.1:.0:.(), 
                                       (-1):.(-1):.0:.(), 1:.(-1):.0:.(),  1:.1:.0:.()]