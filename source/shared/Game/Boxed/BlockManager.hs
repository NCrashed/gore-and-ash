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
{-# LANGUAGE ExistentialQuantification #-}
module Game.Boxed.BlockManager(
    BlockManager()
  , emptyBlockManager
  , registerBlock
  , findBlock
  , getBlockId
  , findBlockById
  , findBlockIdByName
  , module Game.Boxed.SpaceBlock
  , module Game.Boxed.Block
  ) where
  
import Prelude hiding (lookup)
import Game.Boxed.Block
import Game.Boxed.SpaceBlock
import Data.HashMap
import Data.Word

data BlockManager = BlockManager {
     blockMap :: Map String SomeBlock
   , indexBlockMap :: Map Word32 SomeBlock 
   , blockIndexMap :: Map SomeBlock Word32
   }
   
emptyBlockManager :: BlockManager
emptyBlockManager = mng
  where
    Right mng = registerBlock emptyMng SpaceBlock
    emptyMng = BlockManager { blockMap = empty, indexBlockMap = empty, blockIndexMap = empty }

registerBlock :: Block b => BlockManager -> b -> Either String BlockManager
registerBlock mng block = case name `lookup` bmap of
  Just _  -> Left $ "Block with name '" ++ name ++ "' already exists!"
  Nothing -> Right $ mng { 
      blockMap = insert name wrapedBlock bmap 
    , indexBlockMap = indexBlockMap'
    , blockIndexMap = blockIndexMap'}
  where
    bmap = blockMap mng
    name = blockName block
    wrapedBlock = SomeBlock block
    newIndex = fromIntegral $ size bmap
    indexBlockMap' = insert newIndex wrapedBlock $ indexBlockMap mng
    blockIndexMap' = insert wrapedBlock newIndex $ blockIndexMap mng
    
findBlock :: BlockManager -> String -> Maybe SomeBlock
findBlock mng name = name `lookup` blockMap mng

getBlockId :: Block b => BlockManager -> b -> Maybe Word32
getBlockId mng block = do
  name <- blockName block `lookup` blockMap mng
  name `lookup` blockIndexMap mng 

findBlockIdByName :: BlockManager -> String -> Maybe Word32
findBlockIdByName mng name = do
  block <- findBlock mng name
  block `lookup` blockIndexMap mng
  
findBlockById :: BlockManager -> Word32 -> Maybe SomeBlock
findBlockById = undefined