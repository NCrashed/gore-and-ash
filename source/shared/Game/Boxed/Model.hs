-- Copyright 2014 Anton Gushcha
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
{-# LANGUAGE TemplateHaskell #-}
module Game.Boxed.Model(
    BoxedModel()
  , newBoxedModel
  , modelChunkSize
  , toLocalBlocks
  , toGlobalBlocks
  , toLocalChunks
  , toGlobalChunks
  , blockPosFromCoords
  , chunkPosFromCoords
  , globalModelCoords
  , getBlock
  , setBlock
  ) where

import Game.LocatedObject
import Game.Boxed.Chunk as Chunk
import Game.Boxed.ChunkProvider
import Game.Boxed.Block
import Util.Vec()

import Control.Lens
import Data.HashMap as M
import Data.Bits
import Data.Vec as Vec hiding (Map, set)
import Data.Maybe (fromMaybe)

data BoxedModel = BoxedModel {
     _location :: Location
   , _rotation :: Rotation
   , _chunks   :: Map ChunkPosition BoxedChunk
   , _provider :: ChunkProvider
   }
   
type ChunkPosition = Vec3 Int

makeLenses ''BoxedModel

instance LocatedObject BoxedModel where
  getLocation model = model^.location
  getRotation model = model^.rotation
  setLocation = flip $ set location
  setRotation = flip $ set rotation
   
modelChunkSize :: Int
modelChunkSize = 16

toLocalBlocks :: BoxedModel -> Location -> Vec3 Int
toLocalBlocks m v = Vec.map truncate $ toLocal m v

toGlobalBlocks :: BoxedModel -> Vec3 Int -> Location
toGlobalBlocks m v = toGlobal m $ Vec.map fromIntegral v

blockPosFromCoords :: Vec3 Int -> Vec3 Int
blockPosFromCoords = Vec.map (`mod` modelChunkSize)

chunkPosFromCoords :: Vec3 Int -> Vec3 Int
chunkPosFromCoords = Vec.map (.&. modelChunkSize)

globalModelCoords :: Vec3 Int -> Vec3 Int -> Vec3 Int
globalModelCoords chunkPos blockPos = blockPos + Vec.map (* modelChunkSize) chunkPos

toLocalChunks :: BoxedModel -> Location -> Vec3 Int
toLocalChunks m = chunkPosFromCoords . toLocalBlocks m

toGlobalChunks :: BoxedModel -> Vec3 Int -> Location
toGlobalChunks m = toGlobalBlocks m . Vec.map (* modelChunkSize)

newBoxedModel :: Location -> Rotation -> ChunkProvider -> BoxedModel
newBoxedModel loc rot = BoxedModel loc rot empty
 
getBlock :: BoxedModel -> Vec3 Int -> SomeBlock 
getBlock model pos = case chunkPos `M.lookup` (model^.chunks) of
  Nothing    -> getChunkBlock newchunk blockPos
  Just chunk -> getChunkBlock chunk blockPos
  where
    newchunk = (model^.provider^.generator) chunkPos
    chunkPos = blockPosFromCoords pos
    blockPos = chunkPosFromCoords pos
    
setBlock :: Block a => BoxedModel -> Vec3 Int -> a -> BoxedModel
setBlock model pos block = updateChunk $ fromMaybe newchunk $ chunkPos `M.lookup` (model^.chunks)
  where
    updateChunk :: BoxedChunk -> BoxedModel
    updateChunk chunk = (chunks .~ M.insert chunkPos chunk (model^.chunks)) model
    newchunk = setChunkBlock ((model^.provider^.generator) chunkPos) blockPos block
    chunkPos = blockPosFromCoords pos
    blockPos = chunkPosFromCoords pos