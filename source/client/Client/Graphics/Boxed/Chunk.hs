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

import Client.Graphics.Common
import Client.Graphics.PolyCube
import Game.Boxed.Chunk

chunkFrameBuffer :: Texture2D RGBFormat -> BoxedChunk -> Float -> Vec2 Int -> FrameBuffer RGBFormat () ()
chunkFrameBuffer atlas chunk angle size = paintSolid (litChunk atlas chunk angle size) emptyFrameBuffer

litChunk :: Texture2D RGBFormat -> BoxedChunk -> Float -> Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float))
litChunk atlas chunk angle size = fmap (enlight atlas) $ rasterizedChunk chunk angle size
    
rasterizedChunk :: BoxedChunk -> Float -> Vec2 Int -> FragmentStream (Vec3 (Fragment Float), Vec2 (Fragment Float))
rasterizedChunk chunk angle size = rasterizeFront $ transformedChunk chunk angle size

transformedChunk :: BoxedChunk -> Float -> Vec2 Int -> PrimitiveStream Triangle (Vec4 (Vertex Float), (Vec3 (Vertex Float), Vec2 (Vertex Float)))
transformedChunk chunk angle size = fmap (transform angle size) $ chunkTriangles chunk
    
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
            
        neighboursGetters = [upNeighbour,  downNeighbour, forwardNeighbour, rightNeigbour, backNeighbour, leftNeighbour]
        neighboursSides   = [cubeSidePosY, cubeSideNegY,  cubeSidePosX,     cubeSidePosZ,  cubeSideNegX,  cubeSideNegZ]
        
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