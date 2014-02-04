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
module Game.Boxed.ChunkProvider(
    ChunkProvider(..)
  , generator
  ) where
  
import Game.Boxed.Chunk
import Control.Lens
import Data.Vec

data ChunkProvider = ChunkProvider {
    _generator :: Vec3 Int -> BoxedChunk
  }

makeLenses ''ChunkProvider
  