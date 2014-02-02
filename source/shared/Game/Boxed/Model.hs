-- Copyright 2013-2014 Anton Gushcha
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
module Game.Boxed.Model(
    BoxedModel()
  , modelChunkSize
  , toLocalBlocks
  , toGlobalBlocks
  , toLocalChunks
  , toGlobalChunks
  , blockPosFromCoords
  , chunkPosFromCoords
  , globalModelCoords
  , getBlock
  ) where

import Game.LocatedObject
import Game.Boxed.Chunk as Chunk
import Game.Boxed.Block
import Game.Boxed.SpaceBlock
import Math.Quaternion
import Util.Vec()

import Data.HashMap as M
import Data.Bits
import Data.Vec as Vec hiding (Map)

data BoxedModel = BoxedModel (Vec3 Float) (Quaternion Float) (Map ChunkPosition BoxedChunk)
type ChunkPosition = Vec3 Int

instance LocatedObject BoxedModel where
  getLocation (BoxedModel v _ _) = v
  getRotation (BoxedModel _ q _) = q
  setLocation (BoxedModel _ q m) v = BoxedModel v q m
  setRotation (BoxedModel v _ m) q = BoxedModel v q m
   
modelChunkSize :: Int
modelChunkSize = 16

toLocalBlocks :: BoxedModel -> Vec3 Float -> Vec3 Int
toLocalBlocks m v = Vec.map truncate $ toLocal m v

toGlobalBlocks :: BoxedModel -> Vec3 Int -> Vec3 Float
toGlobalBlocks m v = toGlobal m $ Vec.map fromIntegral v

blockPosFromCoords :: Vec3 Int -> Vec3 Int
blockPosFromCoords = Vec.map (`mod` modelChunkSize)

chunkPosFromCoords :: Vec3 Int -> Vec3 Int
chunkPosFromCoords = Vec.map (.&. modelChunkSize)

globalModelCoords :: Vec3 Int -> Vec3 Int -> Vec3 Int
globalModelCoords chunkPos blockPos = blockPos + Vec.map (* modelChunkSize) chunkPos

toLocalChunks :: BoxedModel -> Vec3 Float -> Vec3 Int
toLocalChunks m = chunkPosFromCoords . toLocalBlocks m

toGlobalChunks :: BoxedModel -> Vec3 Int -> Vec3 Float
toGlobalChunks m = toGlobalBlocks m . Vec.map (* modelChunkSize)

getBlock :: BoxedModel -> Vec3 Int -> SomeBlock 
getBlock (BoxedModel _ _ chunks) pos = case chunkPos `M.lookup` chunks of
  Nothing    -> SomeBlock SpaceBlock -- | TODO: run chunk provider to get new chunk here
  Just chunk -> getChunkBlock chunk blockPos
  where
    chunkPos = blockPosFromCoords pos
    blockPos = chunkPosFromCoords pos