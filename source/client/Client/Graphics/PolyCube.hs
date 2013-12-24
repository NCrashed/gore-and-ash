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
module Client.Graphics.PolyCube(
      cubeFrameBuffer
    , cube
    , cubeSidePosX
    , cubeSideNegX
    , cubeSidePosY
    , cubeSideNegY
    , cubeSidePosZ
    , cubeSideNegZ
    ) where
    
import Client.Graphics.GPipe
import Data.Monoid

import Client.Graphics.Common 

cube :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cube = mconcat [cubeSidePosX, cubeSideNegX, cubeSidePosY, cubeSideNegY, cubeSidePosZ, cubeSideNegZ]

cubeSidePosX :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cubeSidePosX = toGPUStream TriangleStrip $ zip3 [1:.0:.0:.(), 1:.1:.0:.(), 1:.0:.1:.(), 1:.1:.1:.()] (repeat (1:.0:.0:.()))    cubeUvCoords

cubeSideNegX :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cubeSideNegX = toGPUStream TriangleStrip $ zip3 [0:.0:.1:.(), 0:.1:.1:.(), 0:.0:.0:.(), 0:.1:.0:.()] (repeat ((-1):.0:.0:.())) cubeUvCoords

cubeSidePosY :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cubeSidePosY = toGPUStream TriangleStrip $ zip3 [0:.1:.1:.(), 1:.1:.1:.(), 0:.1:.0:.(), 1:.1:.0:.()] (repeat (0:.1:.0:.()))    cubeUvCoords

cubeSideNegY :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cubeSideNegY = toGPUStream TriangleStrip $ zip3 [0:.0:.0:.(), 1:.0:.0:.(), 0:.0:.1:.(), 1:.0:.1:.()] (repeat (0:.(-1):.0:.())) cubeUvCoords

cubeSidePosZ :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cubeSidePosZ = toGPUStream TriangleStrip $ zip3 [1:.0:.1:.(), 1:.1:.1:.(), 0:.0:.1:.(), 0:.1:.1:.()] (repeat (0:.0:.1:.()))    cubeUvCoords

cubeSideNegZ :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cubeSideNegZ = toGPUStream TriangleStrip $ zip3 [0:.0:.0:.(), 0:.1:.0:.(), 1:.0:.0:.(), 1:.1:.0:.()] (repeat (0:.0:.(-1):.())) cubeUvCoords

cubeUvCoords :: [Vec2 Float]
cubeUvCoords = [0:.0:.(), 0:.1:.(), 1:.0:.(), 1:.1:.()]    

transformedCube :: Float -> Vec2 Int -> PrimitiveStream Triangle (Vec4 (Vertex Float), (Vec3 (Vertex Float), Vec2 (Vertex Float)))
transformedCube angle size = fmap (transform angle size) cube
    
rasterizedCube :: Float -> Vec2 Int -> FragmentStream (Vec3 (Fragment Float), Vec2 (Fragment Float), FragmentDepth)
rasterizedCube angle size = rasterizeFront $ fmap storeDepth $ transformedCube angle size
    where
        storeDepth (posv@(_:._:.depth:.w:.()), (normv, uv)) = (posv, (normv, uv ,depth/w))


litCube :: Texture2D RGBFormat -> Float -> Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float), FragmentDepth)
litCube tex angle size = fmap (enlight tex) $ rasterizedCube angle size
    
cubeFrameBuffer :: Texture2D RGBFormat -> Float -> Vec2 Int -> FrameBuffer RGBFormat DepthFormat ()
cubeFrameBuffer tex angle size = paintSolid (litCube tex angle size) emptyFrameBuffer