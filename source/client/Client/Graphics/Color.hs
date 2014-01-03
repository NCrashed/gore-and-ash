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
module Client.Graphics.Color(
    rgbaToWord32
  , word32ToRgba
  ) where
  
import Client.Graphics.GPipe
import Data.Word
import Data.Bits
  
rgbaToWord32 :: Color RGBAFormat Word8 -> Word32
rgbaToWord32 (RGBA (red:.green:.blue:.()) alpha) = red' .|. green' .|. blue' .|. alpha'
  where
    alpha' = fromIntegral alpha
    blue'  = shift (fromIntegral blue)   8
    green' = shift (fromIntegral green) 16
    red'   = shift (fromIntegral red)   24
    
word32ToRgba :: Word32 -> Color RGBAFormat Word8
word32ToRgba val = RGBA (red:.green:.blue:.()) alpha
  where
    alpha = fromIntegral $ val .&. 0x000000FF
    blue  = fromIntegral $ (val .&. 0x0000FF00) `shift` (-8)
    green = fromIntegral $ (val .&. 0x00FF0000) `shift` (-16)
    red   = fromIntegral $ (val .&. 0xFF000000) `shift` (-24)