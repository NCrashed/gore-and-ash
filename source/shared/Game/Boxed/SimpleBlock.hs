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
module Game.Boxed.SimpleBlock(
    SimpleBlock()
  , uniformBlock
  , pillarBlock
  ) where
  
import Game.Boxed.Block
import Game.Boxed.Side
import Data.Hashable

-- | The simplest kind of blocks - cubes with textures.
data SimpleBlock = SimpleBlock String (Side -> String)

instance Hashable SimpleBlock where
  hashWithSalt salt (SimpleBlock name _) = hashWithSalt salt name
  
instance Block SimpleBlock where
  blockName (SimpleBlock name _) = name
  blockTexture (SimpleBlock _ f) = f

-- | Create block with one texture for each side
uniformBlock :: String -- ^ Block name, should be unique
  -> String -- ^ Texture resource name 
  -> SimpleBlock
uniformBlock name tex = SimpleBlock name (const tex)

-- | Create block with one texture for plain sides and special for upward and downward directions.
pillarBlock :: String -- ^ Block name, should be unique 
  -> String -- ^ Upward texture 
  -> String -- ^ Downward texture 
  -> String -- ^ Sideward texture 
  -> SimpleBlock
pillarBlock name upTex downTex sideTex = SimpleBlock name $ \case
  Upward   -> upTex
  Downward -> downTex
  _        -> sideTex