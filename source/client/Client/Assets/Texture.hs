-- Copyright 2013 Anton Gushcha, Tobias Bexelius
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
{-# LANGUAGE FlexibleInstances, UndecidableInstances, DeriveDataTypeable, RankNTypes, FlexibleContexts, TypeFamilies, ExistentialQuantification #-}
module Client.Assets.Texture(
    TextureResource(..)
  , ResourceParams(..) 
  ) where
  
import Client.Graphics.GPipe
import Client.Assets.Resource
import qualified Data.ByteString.Lazy     as BZ
import Data.Typeable
import Data.Functor ((<$>))

import qualified Data.Array.Repa.IO.DevIL        as IL
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.ForeignPtr as R
import Foreign.Ptr
import Foreign.ForeignPtr.Safe
import Util.Monad
import System.IO (openTempFile, hClose)
import System.Directory (removeFile, createDirectoryIfMissing)
import Data.Word (Word8)
import Debug.Trace (trace)

data TextureResource = forall f . (ColorFormat f) => TextureResource (Texture2D f)
  deriving (Typeable)

pipeTexture :: String -> BZ.ByteString -> (FilePath -> IO b) -> IO b
pipeTexture resname pipeData action = do
  createDirectoryIfMissing true "temp"
  (name, handle) <- openTempFile "temp" resname
  
  BZ.hPut handle pipeData
  hClose handle
  
  res <- action name
  removeFile name
  return res
    
loadTexture2D :: FilePath -> IO TextureResource
loadTexture2D path = do
  img <- IL.runIL $ IL.readImage path
  case img of
    IL.RGBA arr -> let arr' = swapRGBA arr in 
      trace "1" $ wrapper arr' $ \ptr -> TextureResource <$> newTexture UnsignedInt8_8_8_8              RGBA8      (getSize arr') [ptr] 
    IL.RGB  arr -> let arr' = swapRGB  arr in
      trace "2" $ wrapper arr' $ \ptr -> TextureResource <$> newTexture (PerComp3 UnsignedByteFormat)   RGB8       (getSize arr') [ptr]
    IL.BGRA arr -> let arr' = swapBGRA arr in
      trace "3" $ wrapper arr' $ \ptr -> TextureResource <$> newTexture UnsignedInt8_8_8_8              RGBA8      (getSize arr') [ptr]
    IL.BGR  arr -> let arr' = swapBGR  arr in
      trace "4" $ wrapper arr' $ \ptr -> TextureResource <$> newTexture (PerComp3 UnsignedByteFormat)   RGB8       (getSize arr') [ptr]
      
    IL.Grey arr -> wrapper arr $ \ptr -> TextureResource <$> newTexture UnsignedByteFormat              Luminance8 (getSize arr) [ptr]
  where
    getSize :: (R.Shape d) => R.Array R.F d Word8 -> Vec2 Int
    getSize arr = let shs = reverse $ R.listOfShape $ R.extent arr in (shs !! 1) :. head shs :. ()  
 
    wrapper :: R.Array R.F d Word8 -> (Ptr Word8 -> IO b) -> IO b
    wrapper = withForeignPtr . R.toForeignPtr 

    swapRGBA :: R.Array R.F R.DIM3 Word8 -> R.Array R.F R.DIM3 Word8
    swapRGBA arr = R.computeS $ R.traverse arr id $ \look (R.Z R.:. ri R.:. ci R.:. ch ) -> look $ R.ix3 (rn-ri-1) ci $ if ch < 4 
      then 3 - ch
      else ch
      where (R.Z R.:. rn R.:. _ R.:. _ ) = R.extent arr 
          
    swapRGB :: R.Array R.F R.DIM3 Word8 -> R.Array R.F R.DIM3 Word8
    swapRGB arr = R.computeS $ R.traverse arr id $ \look (R.Z R.:. ri R.:. ci R.:. ch ) -> look $ R.ix3 (rn-ri-1) ci ch
      where (R.Z R.:. rn R.:. _ R.:. _ ) = R.extent arr 
          
    swapBGRA :: R.Array R.F R.DIM3 Word8 -> R.Array R.F R.DIM3 Word8
    swapBGRA arr = R.computeS $ R.traverse arr id swappy
      where
        swappy look (R.Z R.:. ri R.:. ci R.:. ch ) = look $ R.ix3 (rn-ri-1) ci $ case ch of
          0 -> 3  
          1 -> 0
          2 -> 1
          3 -> 2
          _ -> ch    
        (R.Z R.:. rn R.:. _ R.:. _ ) = R.extent arr 
          
    swapBGR :: R.Array R.F R.DIM3 Word8 -> R.Array R.F R.DIM3 Word8
    swapBGR arr = R.computeS $ R.traverse arr id $ \look (R.Z R.:. ri R.:. ci R.:. ch ) -> look $ R.ix3 (rn-ri-1) ci $ if ch < 3 
      then 2 - ch
      else ch
      where (R.Z R.:. rn R.:. _ R.:. _ ) = R.extent arr 
                  
instance Resource TextureResource where
    data ResourceParams TextureResource = TextureNoParams
    loadResource name TextureNoParams bs = liftExceptions $ pipeTexture name bs loadTexture2D
    saveResource = undefined