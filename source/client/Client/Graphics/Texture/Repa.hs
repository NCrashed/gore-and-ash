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
module Client.Graphics.Texture.Repa(
    fromTexture
  , toTexture
  , cacheTexture
  ) where

import Client.Graphics.GPipe 
import Client.Graphics.Texture.Render 
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as R
import Data.Word
import Data.Functor
import Foreign.ForeignPtr
import Debug.Trace (trace)

fromTexture :: Vec2 Int -> Texture2D RGBAFormat -> IO (R.Array R.F R.DIM3 Word8)
fromTexture size tex = R.fromForeignPtr (getShape size) <$> loadBuffer
  where
    framebuffer = renderTexture True tex (0:.0:.()) (1:.1:.())
    
    getShape :: Vec2 Int -> R.DIM3
    getShape (sx:.sy:.()) = R.ix3 sy sx 4 
    
    loadBuffer :: IO (ForeignPtr Word8)
    loadBuffer = do
      fptr <- mallocForeignPtrBytes bytes :: IO (ForeignPtr Word8)
      withForeignPtr fptr $ trace (show size) $ getFrameBufferColor UnsignedInt8_8_8_8 size framebuffer
      trace "!" $ return fptr
      where
        bytes = R.size $ getShape size
      
toTexture :: R.Array R.F R.DIM3 Word8 -> IO (Texture2D RGBAFormat)
toTexture arr = withForeignPtr (R.toForeignPtr arr) $ \ptr -> newTexture UnsignedInt8_8_8_8 RGBA8 size [ptr]
  where
    size = let shs = reverse $ R.listOfShape $ R.extent arr in (shs !! 1) :. head shs :. ()

cacheTexture :: Vec2 Int -> Texture2D RGBAFormat -> IO (Texture2D RGBAFormat)
cacheTexture size tex = do
  arr <- fromTexture size tex 
  toTexture arr