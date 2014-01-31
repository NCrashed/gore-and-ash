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
{-# LANGUAGE LambdaCase #-}
module Game.Boxed.SpaceBlock(
    SpaceBlock(..)
  ) where
  
import Game.Boxed.Block
import Data.Hashable

-- | Special block type that represents empty space
data SpaceBlock = SpaceBlock

instance Hashable SpaceBlock where
  hashWithSalt salt _ = salt
  
instance Block SpaceBlock where
  blockName    = const "Nothing"
  blockTexture = const . const ""