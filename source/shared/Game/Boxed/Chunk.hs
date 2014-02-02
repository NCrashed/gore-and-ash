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
module Game.Boxed.Chunk(
      BoxedChunk()
    , chunkSize
    , chunkSizeVec
    , chunkFromList
    , chunkBlockMng
    
    , getRawData
    
    , Neighbours()
    , getNeighbour
    , chunkFlatMapWithNeighbours
    
    , readBlockId
    , getChunkBlock
    ) where
    
import qualified Data.Array.Repa as Repa
import Data.Vec as Vec hiding (head, foldl, length)
import Data.Word
import Data.Functor
import Foreign (ForeignPtr, mallocForeignPtr)
import Data.Array.Repa.Repr.ForeignPtr (computeIntoP)

import Game.Boxed.BlockManager
import Game.Boxed.Side
import Util.Convert


data BoxedChunk = BoxedChunk
  -- | Array with block ids 
  (Repa.Array Repa.U Repa.DIM3 Word32)
  -- | Block manager for mapping blocks ids and textures
  BlockManager
  
chunkSize :: BoxedChunk -> Int
chunkSize (BoxedChunk array _) = if allEqual then head dimList else error "Chunk is not a cube!"
    where
        dimList = Repa.listOfShape $ Repa.extent array
        allEqual = fst $ foldl (\(a, l) e -> (a && l == e, e)) (True, head dimList) dimList
        
chunkFromList :: Int -> BlockManager -> [String] -> Maybe BoxedChunk
chunkFromList sideSize mng list = if sideSize ^ (3 :: Int) /= length list then Nothing 
  else chunk <$> sequence (findBlockIdByName mng <$> list)
  where 
    chunk ids = BoxedChunk (Repa.fromListUnboxed (Repa.ix3 sideSize sideSize sideSize) ids) mng
        
chunkSizeVec :: BoxedChunk -> Vec3 Int
chunkSizeVec (BoxedChunk array _) = toVec $ Repa.extent array 
   
getRawData :: BoxedChunk -> IO (ForeignPtr Word32)
getRawData (BoxedChunk array _) = do
  ptr <- mallocForeignPtr :: IO (ForeignPtr Word32)
  computeIntoP ptr $ Repa.delay array 
  return ptr

chunkBlockMng :: BoxedChunk -> BlockManager
chunkBlockMng (BoxedChunk _ mng) = mng

type Neighbours = Vec6 Word32

getNeighbour :: Neighbours -> Side -> Word32
getNeighbour = flip get'
  where
    get' Upward    = get n0
    get' Downward  = get n1
    get' Forward   = get n2
    get' Backward  = get n3
    get' Leftward  = get n4
    get' Rightward = get n5 
    get' _         = const 0
        
chunkFlatMapWithNeighbours :: (Word32 -> Neighbours -> Vec3 Int -> b) -> BoxedChunk -> [b]
chunkFlatMapWithNeighbours fun (BoxedChunk array _) = Repa.toList $ Repa.traverse array id traverseFunc
  where
    traverseFunc lookFunc index = fun (lookFunc index) neigbours $ toVec index
      where
        neigbours = Vec.fromList $ fmap ((\v -> if inBoundes v then lookFunc v else 0) . Repa.addDim index) 
                        $ toShape . sideDirection <$> [Upward ..]
        inBoundes = Repa.inShape (Repa.extent array)

-- | Returns block id located in specified location.
readBlockId :: BoxedChunk -> Vec3 Int -> Word32
readBlockId (BoxedChunk array _) = Repa.index array . toShape

-- | Returns block located in specified location. If manager don't
-- know id, default empty block is returned.     
getChunkBlock :: BoxedChunk -> Vec3 Int -> SomeBlock
getChunkBlock (BoxedChunk array mng) = maybe (SomeBlock SpaceBlock) SomeBlock . findBlockById mng . Repa.index array . toShape