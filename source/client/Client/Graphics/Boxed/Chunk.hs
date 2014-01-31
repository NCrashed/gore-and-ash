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
    , chunkTriangles
    , ChunkModel(..)
    , buildChunkModel
    ) where
 
import Prelude as P   
import Control.Applicative
import Control.Monad.Trans.Either
import Data.Vec as Vec
import Data.Monoid
import Data.Word
import Data.Maybe
import Data.Sequence as S

import Client.Graphics.GPipe
import Client.Graphics.Common
import Client.Graphics.Mesh
import Client.Graphics.Texture.Atlas
import Client.Assets.Manager
import Game.Boxed.Side
import Game.Boxed.Chunk
import Game.Boxed.BlockManager
import Client.Graphics.GPipe.Inner.Formats (toColor)
import Client.Graphics.PolyCube (cube)
import Debug.Trace (trace)

data ChunkModel = ChunkModel Mesh Atlas

buildChunkAtlas :: BoxedChunk -> ResourceManager -> EitherT String IO (Atlas, ResourceManager)
buildChunkAtlas chunk = renderAtlas atlas 
  where
    atlas = updateAtlas (emptyAtlas 128) $ blockManagerTextures $ chunkBlockMng chunk 
  
buildChunkModel :: BoxedChunk -> ResourceManager -> EitherT String IO (ChunkModel, ResourceManager)
buildChunkModel chunk resMng = do
   (atlas, resMng') <- buildChunkAtlas chunk resMng
   let model = chunkTriangles chunk atlas
   return $ (ChunkModel model atlas, resMng')
   
chunkFrameBuffer :: ChunkModel -> Float -> Vec2 Int -> FrameBuffer RGBAFormat DepthFormat ()
chunkFrameBuffer model angle size = paintSolidDepthAlpha (litChunk model angle size) emptyFrameBufferDepthAlpha

litChunk :: ChunkModel -> Float -> Vec2 Int -> FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth)
litChunk (ChunkModel mesh atlas) angle size = fmap texturise $ rasterizedChunk mesh angle size
  where
    texturise (normv, uv, depth) = (sample (Sampler Linear Wrap) (atlasTexture atlas) uv, depth) --(toColor (0:.0.45:.1:.1:.()), depth)

rasterizedChunk :: Mesh -> Float -> Vec2 Int -> FragmentStream (Vec3 (Fragment Float), Vec2 (Fragment Float), FragmentDepth)
rasterizedChunk chunk angle size = fmap storeDepth $ rasterizeFront $ transformedChunk chunk angle size
    where
        storeDepth (normv, uv) = (normv, uv , fragDepth) 
        
transformedChunk :: Mesh -> Float -> Vec2 Int -> PrimitiveStream Triangle (Vec4 (Vertex Float), (Vec3 (Vertex Float), Vec2 (Vertex Float)))
transformedChunk chunk angle size = fmap (transform angle size) $ renderMesh chunk
    
chunkTriangles :: BoxedChunk -> Atlas -> Mesh
chunkTriangles chunk atlas = mconcat $ chunkFlatMapWithNeighbours genBorders chunk
    where
        genBorders :: Word32 -> Neighbours -> Vec3 Int -> Mesh
        genBorders val neighbours pos
            | val == 0  = mempty    
            | otherwise = mconcat $ fmap (genSide val pos neighbours) [Upward ..]
            
        genSide :: Word32 -> Vec3 Int -> Neighbours -> Side -> Mesh
        genSide origVal pos neighbours side
            | val /= 0  = mempty
            | otherwise = fmap (sideTransform uvOrigin uvSize pos) sideMesh
            where
              val = getNeighbour neighbours side
              sideMesh = getSideMesh side
              (uvOrigin, uvSize) = fromMaybe (0:.0:.(), 1:.1:.()) $ textureUvInAtlas atlas =<< flip blockTexture side <$> findBlockById manager origVal
                   
        manager = chunkBlockMng chunk
        
        sideTransform :: Vec2 Float -> Vec2 Float -> Vec3 Int -> (Vec3 Float, Vec3 Float, Vec2 Float) -> (Vec3 Float, Vec3 Float, Vec2 Float)
        sideTransform uvOrigin uvSize (cx:.cy:.cz:.()) (pos, normv, uv) = (Vec.take n3 newPos, normv, uv')
            where
            cellSize :: Vec3 Float
            cellSize = 1.0 / fromIntegral (chunkSize chunk)
            chunkPos :: Vec3 Float
            chunkPos = Vec.fromList $ fmap fromIntegral [cx, cy, cz]
            transMat :: Mat44 Float
            transMat = scaling cellSize `multmm` translation chunkPos
            newPos = transMat `multmv` (homPoint pos :: Vec4 Float)
            uv' = remapCoords uvOrigin uvSize uv
            
-- | Coordinates transformation from viewport system to specified local. This function maps points in particular
-- region into texture coordinates.
remapCoords :: Vec2 Float -> Vec2 Float -> Vec2 Float -> Vec2 Float
remapCoords (ox:.oy:.()) (sx:.sy:.()) (ux:.uy:.()) = ux':.uy':.()
  where
    ux' = ux * sx + ox
    uy' = uy * sy + oy
    
getSideMesh :: Side -> Mesh
getSideMesh = MeshContainer TriangleStrip . S.fromList . side
  where
  uvs = [0:.0:.(), 0:.1:.(), 1:.0:.(), 1:.1:.()]
  side Upward    = P.zip3 [0:.1:.1:.(), 1:.1:.1:.(), 0:.1:.0:.(), 1:.1:.0:.()] (repeat (0:.1:.0:.()))    uvs 
  side Downward  = P.zip3 [0:.0:.0:.(), 1:.0:.0:.(), 0:.0:.1:.(), 1:.0:.1:.()] (repeat (0:.(-1):.0:.())) uvs
  side Forward   = P.zip3 [1:.0:.0:.(), 1:.1:.0:.(), 1:.0:.1:.(), 1:.1:.1:.()] (repeat (1:.0:.0:.()))    uvs
  side Backward  = P.zip3 [0:.0:.1:.(), 0:.1:.1:.(), 0:.0:.0:.(), 0:.1:.0:.()] (repeat ((-1):.0:.0:.())) uvs
  side Rightward = P.zip3 [1:.0:.1:.(), 1:.1:.1:.(), 0:.0:.1:.(), 0:.1:.1:.()] (repeat (0:.0:.1:.()))    uvs
  side Leftward  = P.zip3 [0:.0:.0:.(), 0:.1:.0:.(), 1:.0:.0:.(), 1:.1:.0:.()] (repeat (0:.0:.(-1):.())) uvs
  side _ = []        