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
      BlockLocation
    , BoxedChunk()
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
    , setChunkBlock
    ) where
    
import qualified Data.Array.Repa as R
import Data.Vec as Vec hiding (head, foldl, length)
import Data.Word
import Data.Functor
import Foreign (ForeignPtr, mallocForeignPtr)
import Data.Array.Repa.Repr.ForeignPtr (computeIntoP)

import Game.Boxed.BlockManager
import Game.Boxed.Side
import Util.Convert
import Data.Maybe (fromJust)

type BlockLocation = Vec3 Int
type IdsArray = R.Array R.U R.DIM3 Word32

data BoxedChunk = BoxedChunk
  -- | Array with block ids 
  IdsArray
  -- | Block manager for mapping blocks ids and textures
  BlockManager
  
chunkSize :: BoxedChunk -> Int
chunkSize (BoxedChunk array _) = if allEqual then head dimList else error "Chunk is not a cube!"
    where
        dimList = R.listOfShape $ R.extent array
        allEqual = fst $ foldl (\(a, l) e -> (a && l == e, e)) (True, head dimList) dimList
        
chunkFromList :: Int -> BlockManager -> [String] -> Maybe BoxedChunk
chunkFromList sideSize mng list = if sideSize ^ (3 :: Int) /= length list then Nothing 
  else chunk <$> sequence (findBlockIdByName mng <$> list)
  where 
    chunk ids = BoxedChunk (R.fromListUnboxed (R.ix3 sideSize sideSize sideSize) ids) mng
        
chunkSizeVec :: BoxedChunk -> Vec3 Int
chunkSizeVec (BoxedChunk array _) = toVec $ R.extent array 
   
getRawData :: BoxedChunk -> IO (ForeignPtr Word32)
getRawData (BoxedChunk array _) = do
  ptr <- mallocForeignPtr :: IO (ForeignPtr Word32)
  computeIntoP ptr $ R.delay array 
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
        
chunkFlatMapWithNeighbours :: (Word32 -> Neighbours -> BlockLocation -> b) -> BoxedChunk -> [b]
chunkFlatMapWithNeighbours fun (BoxedChunk array _) = R.toList $ R.traverse array id traverseFunc
  where
    traverseFunc lookFunc index = fun (lookFunc index) neigbours $ toVec index
      where
        neigbours = Vec.fromList $ fmap ((\v -> if inBoundes v then lookFunc v else 0) . R.addDim index) 
                        $ toShape . sideDirection <$> [Upward ..]
        inBoundes = R.inShape (R.extent array)

-- | Returns block id located in specified location.
readBlockId :: BoxedChunk -> BlockLocation -> Word32
readBlockId (BoxedChunk array _) = R.index array . toShape

-- | Returns block located in specified location. If manager don't
-- know id, default empty block is returned.     
getChunkBlock :: BoxedChunk -> BlockLocation -> SomeBlock
getChunkBlock (BoxedChunk array mng) = maybe (SomeBlock SpaceBlock) SomeBlock . findBlockById mng . R.index array . toShape

-- | Writes single block id located in specified location.
-- TODO: Check if there is a better way to write single element in an array...
writeBlockId :: IdsArray -> BlockLocation -> Word32 -> IdsArray
writeBlockId array pos val = R.computeS $ R.traverse array id updateElement
  where
   {-# INLINE updateElement #-}
   updateElement lookf i = if i == toShape pos then val else lookf i  

-- | Update block in specified location. If there is block names conflict,
-- will throw.   
setChunkBlock :: Block a => BoxedChunk -> BlockLocation -> a -> BoxedChunk
setChunkBlock (BoxedChunk array mng) pos block = BoxedChunk newarray newmng
  where
    (bid, newmng) = case getBlockId mng block of
      Nothing -> case registerBlock mng block of
        Right mng' -> (fromJust $ getBlockId mng' block, mng')
        Left er -> error er
      Just id' -> (id', mng)
    newarray = writeBlockId array pos bid