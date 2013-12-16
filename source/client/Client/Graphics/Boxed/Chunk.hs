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
module Client.Graphics.Boxed.Chunk(
      chunkFrameBuffer
    ) where
    
import Graphics.GPipe
import Control.Applicative
import Data.Vec as Vec
import Data.Monoid
import Data.Word

import Game.Boxed.Chunk

chunkFrameBuffer :: Texture2D RGBFormat -> BoxedChunk -> Float -> Vec2 Int -> FrameBuffer RGBFormat () ()
chunkFrameBuffer atlas chunk angle size = paintSolid (litChunk atlas chunk angle size) emptyFrameBuffer

paintSolid :: FragmentStream (Color RGBFormat (Fragment Float)) -> FrameBuffer RGBFormat () () -> FrameBuffer RGBFormat () ()
paintSolid = paintColor NoBlending (RGB $ vec True)

emptyFrameBuffer :: FrameBuffer RGBFormat () ()
emptyFrameBuffer = newFrameBufferColor (RGB 0)

litChunk :: Texture2D RGBFormat -> BoxedChunk -> Float -> Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float))
litChunk atlas chunk angle size = fmap (enlight atlas) $ rasterizedChunk chunk angle size

enlight :: Texture2D RGBFormat -> (Vec3 (Fragment Float), Vec2 (Fragment Float)) -> Color RGBFormat (Fragment Float)
enlight tex (normv, uv) = RGB (c * vec (normv `dot` toGPU (0:.0:.1:.())))
    where RGB c = sample (Sampler Linear Wrap) tex uv
    
rasterizedChunk :: BoxedChunk -> Float -> Vec2 Int -> FragmentStream (Vec3 (Fragment Float), Vec2 (Fragment Float))
rasterizedChunk chunk angle size = rasterizeFront $ transformedChunk chunk angle size

transformedChunk :: BoxedChunk -> Float -> Vec2 Int -> PrimitiveStream Triangle (Vec4 (Vertex Float), (Vec3 (Vertex Float), Vec2 (Vertex Float)))
transformedChunk chunk angle size = fmap (transform angle size) $ chunkTriangles chunk

transform :: Float -> Vec2 Int -> (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float)) -> (Vec4 (Vertex Float), (Vec3 (Vertex Float), Vec2 (Vertex Float)))
transform angle (width:.height:.()) (pos, normv, uv) = (transformedPos, (transformedNorm, uv))
    where
    modelMat = rotationVec (normalize (1:.0.5:.0.3:.())) angle `multmm` translation (-0.5)
    viewMat  = translation (-(0:.0:.2:.()))
    projMat  = perspective 1 100 (pi/3) (fromIntegral width / fromIntegral height)
    viewProjMat = projMat `multmm` viewMat
    transformedPos  = toGPU (viewProjMat `multmm` modelMat) `multmv` (homPoint pos :: Vec4 (Vertex Float))
    transformedNorm = toGPU (mat4ToMat3 modelMat) `multmv` normv
    mat4ToMat3 = Vec.map (Vec.take n3) . Vec.take n3
    
chunkTriangles :: BoxedChunk -> PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
chunkTriangles chunk = mconcat $ chunkFlatMapWithNeighbours genBorders chunk
    where
        genBorders :: Word32 -> Neighbours -> Vec3 Int -> PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
        genBorders val neighbours pos
            | val == 0  = mempty    
            | otherwise = mconcat $ fmap (genSide pos) $ zip neighboursSides $ neighboursGetters <*> pure neighbours
        genSide :: Vec3 Int -> (PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float)), Word32) -> PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
        genSide pos (side, val)
            | val /= 0  = mempty
            | otherwise = fmap (sideTransform pos) side
            
        neighboursGetters = [upNeighbour, downNeighbour, forwardNeighbour, rightNeigbour, backNeighbour, leftNeighbour]
        neighboursSides   = [sidePosY,    sideNegY,      sidePosX,         sidePosZ,      sideNegX,      sideNegZ]
        
        sideTransform :: Vec3 Int -> (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float)) -> (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
        sideTransform (cx:.cy:.cz:.()) (pos, normv, uv) = (Vec.take n3 newPos, normv, uv)
            where
            cellSize :: Vec3 Float
            cellSize = 1.0 / fromIntegral (chunkSize chunk)
            chunkPos :: Vec3 Float
            chunkPos = fromList $ fmap fromIntegral [cx, cy, cz]
            transMat :: Mat44 (Vertex Float)
            transMat = toGPU $ scaling cellSize `multmm` translation chunkPos
            newPos = transMat `multmv` (homPoint pos :: Vec4 (Vertex Float))
        
        sidePosX = toGPUStream TriangleStrip $ zip3 [1:.0:.0:.(), 1:.1:.0:.(), 1:.0:.1:.(), 1:.1:.1:.()] (repeat (1:.0:.0:.()))    uvCoords
        sideNegX :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
        sideNegX = toGPUStream TriangleStrip $ zip3 [0:.0:.1:.(), 0:.1:.1:.(), 0:.0:.0:.(), 0:.1:.0:.()] (repeat ((-1):.0:.0:.())) uvCoords
        sidePosY = toGPUStream TriangleStrip $ zip3 [0:.1:.1:.(), 1:.1:.1:.(), 0:.1:.0:.(), 1:.1:.0:.()] (repeat (0:.1:.0:.()))    uvCoords
        sideNegY = toGPUStream TriangleStrip $ zip3 [0:.0:.0:.(), 1:.0:.0:.(), 0:.0:.1:.(), 1:.0:.1:.()] (repeat (0:.(-1):.0:.())) uvCoords
        sidePosZ = toGPUStream TriangleStrip $ zip3 [1:.0:.1:.(), 1:.1:.1:.(), 0:.0:.1:.(), 0:.1:.1:.()] (repeat (0:.0:.1:.()))    uvCoords
        sideNegZ = toGPUStream TriangleStrip $ zip3 [0:.0:.0:.(), 0:.1:.0:.(), 1:.0:.0:.(), 1:.1:.0:.()] (repeat (0:.0:.(-1):.())) uvCoords
        uvCoords = [0:.0:.(), 0:.1:.(), 1:.0:.(), 1:.1:.()]    
        