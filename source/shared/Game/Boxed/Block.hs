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
{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
module Game.Boxed.Block(
    Block(..)
  , SomeBlock(..)
  , blockTextures
  ) where

import Game.Boxed.Side
import Data.Hashable
import Data.Typeable
import Data.Functor
import Data.List

-- | Class for block materials. Material type describes all shared
-- properties of blocks including name, textures, shape and behavior.
class (Hashable a, Eq a) => Block a where
  blockName    :: a -> String
  blockTexture :: a -> Side -> String
  
data SomeBlock = forall a . (Block a) => SomeBlock a 
  deriving (Typeable)

instance Block SomeBlock where
  blockName (SomeBlock b) = blockName b
  blockTexture (SomeBlock b) = blockTexture b
  
instance Hashable SomeBlock where
  hashWithSalt salt (SomeBlock b) = hashWithSalt salt b

instance Eq SomeBlock where
  (SomeBlock a) == (SomeBlock b) = blockName a == blockName b
  
instance Ord SomeBlock where
  (SomeBlock a) >= (SomeBlock b) = blockName a >= blockName b

-- | Returns all textures used by the block.
blockTextures :: Block a => a -> [String]
blockTextures block = nub $ blockTexture block <$> [Upward .. ]