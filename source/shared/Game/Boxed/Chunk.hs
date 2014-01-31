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
module Game.Boxed.Chunk(
      BoxedChunk()
    , chunkSize
    , chunkSizeVec
    , chunkFromList
    
    , getRawData
    
    , Neighbours()
    , upNeighbour
    , downNeighbour
    , forwardNeighbour
    , rightNeigbour
    , backNeighbour
    , leftNeighbour
    , chunkFlatMapWithNeighbours
    ) where
    
import Game.Boxed.BlockManager
import qualified Data.Array.Repa as Repa
import Data.Vec as Vec hiding (head, foldl, length)
import Data.Word
import Data.Functor
import Foreign (ForeignPtr, mallocForeignPtr)
import Data.Array.Repa.Repr.ForeignPtr (computeIntoP)

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
chunkSizeVec (BoxedChunk array _) = dimX :. dimY :. dimZ :. () 
  where
    (Repa.Z Repa.:. dimX Repa.:. dimY Repa.:. dimZ) = Repa.extent array
   
getRawData :: BoxedChunk -> IO (ForeignPtr Word32)
getRawData (BoxedChunk array _) = do
  ptr <- mallocForeignPtr :: IO (ForeignPtr Word32)
  computeIntoP ptr $ Repa.delay array 
  return ptr

type Neighbours = Vec6 Word32

upNeighbour :: Neighbours -> Word32
upNeighbour = get n0

downNeighbour :: Neighbours -> Word32
downNeighbour = get n1

forwardNeighbour :: Neighbours -> Word32
forwardNeighbour = get n2

rightNeigbour :: Neighbours -> Word32
rightNeigbour = get n3

backNeighbour :: Neighbours -> Word32
backNeighbour = get n4

leftNeighbour :: Neighbours -> Word32
leftNeighbour = get n5
        
chunkFlatMapWithNeighbours :: (Word32 -> Neighbours -> Vec3 Int -> b) -> BoxedChunk -> [b]
chunkFlatMapWithNeighbours fun (BoxedChunk array _) = Repa.toList $ Repa.traverse array id traverseFunc
    where
        traverseFunc lookFunc index = fun (lookFunc index) neigbours $ fromShape index
            where
                neigbours = Vec.fromList $ fmap ((\v -> if inBoundes v then lookFunc v else 0) . Repa.addDim index) 
                                [up, down, forward, right, back, left]
                inBoundes = Repa.inShape (Repa.extent array)
                up      = coord   0   1   0
                down    = coord   0 (-1)  0
                forward = coord   1   0   0
                right   = coord   0   0   1
                back    = coord (-1)  0   0
                left    = coord   0   0 (-1)
                coord = Repa.ix3
                fromShape (Repa.Z Repa.:. x  Repa.:. y Repa.:. z ) = x :. y :. z :. ()