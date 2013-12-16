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
    , chunkFromList
    , Neighbours()
    , upNeighbour
    , downNeighbour
    , forwardNeighbour
    , rightNeigbour
    , backNeighbour
    , leftNeighbour
    , chunkFlatMapWithNeighbours
    ) where
    
import qualified Data.Array.Repa as Repa
import Data.Vec (Vec3, Vec6, fromList, get, n0, n1, n2, n3, n4, n5)
import Data.Word

data BoxedChunk = BoxedChunk (Repa.Array Repa.U Repa.DIM3 Word32)

chunkSize :: BoxedChunk -> Int
chunkSize (BoxedChunk array) = if allEqual then head dimList else error "Chunk is not a cube!"
    where
        dimList = Repa.listOfShape $ Repa.extent array
        allEqual = fst $ foldl (\(a, l) e -> (a && l == e, e)) (True, head dimList) dimList
        
chunkFromList :: Int -> [Word32] -> Maybe BoxedChunk
chunkFromList size list = if size*size*size == length list then Just $ chunk list else Nothing
    where 
        chunk = BoxedChunk . Repa.fromListUnboxed (Repa.Z Repa.:. (size :: Int) Repa.:. (size :: Int) Repa.:. (size :: Int))
        
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
chunkFlatMapWithNeighbours fun (BoxedChunk array) = Repa.toList $ Repa.traverse array id traverseFunc
    where
        traverseFunc lookFunc index = fun (lookFunc index) neigbours $ fromShape index
            where
                neigbours = fromList $ fmap ((\v -> if inBoundes v then lookFunc v else 0) . Repa.addDim index) 
                                [up, down, forward, right, back, left]
                inBoundes = Repa.inShape (Repa.extent array)
                up      = coord   0   1   0
                down    = coord   0 (-1)  0
                forward = coord   1   0   0
                right   = coord   0   0   1
                back    = coord (-1)  0   0
                left    = coord   0   0 (-1)
                coord x y z = Repa.Z Repa.:. (x :: Int) Repa.:. (y :: Int) Repa.:. (z :: Int)
                fromShape (Repa.Z Repa.:. x  Repa.:. y Repa.:. z ) = fromList [x, y, z]