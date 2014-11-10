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
{-# LANGUAGE StandaloneDeriving, FlexibleInstances, UndecidableInstances, DeriveDataTypeable, RankNTypes, FlexibleContexts, TypeFamilies #-}
module Client.Assets.Texture(
    TextureResource(..)
  , TextureResource3D(..)
  , ResourceParams(..) 
  ) where
  
import Graphics.GPipe
import Client.Assets.Resource
import Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy     as BZ
import Util.ByteString
import Data.Typeable

import Codec.Image.STB
import Foreign.Ptr
import Data.Bitmap.Pure
import Util.Monad

newtype TextureResource t = TextureResource t
  deriving (Typeable)
  
newtype TextureResource3D t = TextureResource3D t
  deriving (Typeable)
  
type BitmapProcessor a = PixelComponent t => Size -> NChn -> Padding -> Ptr t -> IO a

loadTexture' :: Int -> BitmapProcessor a -> BZ.ByteString -> IO a
loadTexture' comp io bs = do 
  image <- decodeImage' comp (toStrict bs)
  either (ioError . userError) (`withBitmap` io) image

texture3DFromImage :: (ColorFormat f) => CPUFormat f -> f -> BitmapProcessor (Texture3D f)        
texture3DFromImage cpufmt fmt s@(w,h) _ 0 ptr =
    case quotRem h w of
        (q, 0) -> newTexture cpufmt fmt (w:.w:.q:.()) [ptr]
        _      -> ioError $ userError ("loadTexture: Bad 3D image size " ++ show s)
texture3DFromImage _ _ _ _ _ _ = ioError $ userError "loadTexture: Row padding is not supported"

texture2DFromImage :: (ColorFormat f) => CPUFormat f -> f -> BitmapProcessor (Texture2D f)   
texture2DFromImage cpufmt fmt (w,h) _ 0 ptr = newTexture cpufmt fmt (w:.h:.()) [ptr]
texture2DFromImage _ _ _ _ _ _ = ioError $ userError "loadTexture: Row padding is not supported"

texture1DFromImage :: (ColorFormat f) => CPUFormat f -> f -> BitmapProcessor (Texture1D f) 
texture1DFromImage cpufmt fmt (w,h) _ 0 ptr = newTexture cpufmt fmt (w*h) [ptr]
texture1DFromImage _ _ _ _ _ _ = ioError $ userError "loadTexture: Row padding is not supported"

textureCubeFromImage :: (ColorFormat f) => CPUFormat f -> f -> BitmapProcessor (TextureCube f) 
textureCubeFromImage cpufmt fmt s@(w,h) _ 0 ptr =
    case quotRem h 6 of
        (q, 0) -> newTexture cpufmt fmt (w:.q:.()) [ptr `plusPtr` (off*w*q) | off <- [0..5]]
        _      -> ioError $ userError ("loadTexture: Bad cube image size " ++ show s)
textureCubeFromImage _ _ _ _ _ _ = ioError $ userError "loadTexture: Row padding is not supported"

texture3DFromImage' :: (ColorFormat f) => Int -> CPUFormat f -> f -> BitmapProcessor (Texture3D f)    
texture3DFromImage' d cpufmt fmt s@(w,h) _ 0 ptr =
    case quotRem h d of
        (q, 0) -> newTexture cpufmt fmt (w:.q:.d:.()) [ptr]
        _      -> ioError $ userError ("loadTexture: Bad 3D image size " ++ show s)
texture3DFromImage' _ _ _ _ _ _ _ = ioError $ userError "loadTexture: Row padding is not supported"
  
wrapTexResource :: IO t -> EitherT String IO (TextureResource t)
wrapTexResource io = right . TextureResource =<< liftExceptions io

wrapTex3DResource :: IO t -> EitherT String IO (TextureResource3D t)
wrapTex3DResource io = right . TextureResource3D =<< liftExceptions io

deriving instance Typeable Texture2D
deriving instance Typeable Texture1D
deriving instance Typeable Texture3D
deriving instance Typeable TextureCube
deriving instance Typeable AlphaFormat
deriving instance Typeable LuminanceFormat
deriving instance Typeable LuminanceAlphaFormat
deriving instance Typeable RGBFormat
deriving instance Typeable RGBAFormat

instance Resource (TextureResource (Texture3D AlphaFormat)) where
    data ResourceParams (TextureResource (Texture3D AlphaFormat)) = Par3DAlpha AlphaFormat
    loadResource _ (Par3DAlpha fmt) bs = wrapTexResource $ loadTexture' 1 (texture3DFromImage UnsignedByteFormat fmt) bs 
    saveResource = undefined
instance Resource (TextureResource (Texture3D LuminanceFormat)) where
    data ResourceParams (TextureResource (Texture3D LuminanceFormat)) = Par3DLuminance LuminanceFormat
    loadResource _ (Par3DLuminance fmt) bs = wrapTexResource $ loadTexture' 1 (texture3DFromImage UnsignedByteFormat fmt) bs
    saveResource = undefined
instance Resource (TextureResource (Texture3D LuminanceAlphaFormat)) where
    data ResourceParams (TextureResource (Texture3D LuminanceAlphaFormat)) = Par3DLuminanceAlpha LuminanceAlphaFormat
    loadResource _ (Par3DLuminanceAlpha fmt) bs = wrapTexResource $ loadTexture' 2 (texture3DFromImage (PerComp2 UnsignedByteFormat) fmt) bs
    saveResource = undefined
instance Resource (TextureResource (Texture3D RGBFormat)) where
    data ResourceParams (TextureResource (Texture3D RGBFormat)) = Par3DRGB RGBFormat
    loadResource _ (Par3DRGB fmt) bs = wrapTexResource $ loadTexture' 3 (texture3DFromImage (PerComp3 UnsignedByteFormat) fmt) bs
    saveResource = undefined
instance Resource (TextureResource (Texture3D RGBAFormat)) where
    data ResourceParams (TextureResource (Texture3D RGBAFormat)) = Par3DRGBA RGBAFormat
    loadResource _ (Par3DRGBA fmt) bs = wrapTexResource $ loadTexture' 4 (texture3DFromImage (PerComp4 UnsignedByteFormat) fmt) bs
    saveResource = undefined

instance Resource (TextureResource (Texture2D AlphaFormat)) where
    data ResourceParams (TextureResource (Texture2D AlphaFormat)) = Par2DAlpha AlphaFormat
    loadResource _ (Par2DAlpha fmt) bs = wrapTexResource $ loadTexture' 1 (texture2DFromImage UnsignedByteFormat fmt) bs
    saveResource = undefined
instance Resource (TextureResource (Texture2D LuminanceFormat)) where
    data ResourceParams (TextureResource (Texture2D LuminanceFormat)) = Par2DLuminance LuminanceFormat
    loadResource _ (Par2DLuminance fmt) bs = wrapTexResource $ loadTexture' 1 (texture2DFromImage UnsignedByteFormat fmt) bs
    saveResource = undefined
instance Resource (TextureResource (Texture2D LuminanceAlphaFormat)) where
    data ResourceParams (TextureResource (Texture2D LuminanceAlphaFormat)) = Par2DLuminanceAlpha LuminanceAlphaFormat
    loadResource _ (Par2DLuminanceAlpha fmt) bs = wrapTexResource $ loadTexture' 2 (texture2DFromImage (PerComp2 UnsignedByteFormat) fmt) bs
    saveResource = undefined
instance Resource (TextureResource (Texture2D RGBFormat)) where 
    data ResourceParams (TextureResource (Texture2D RGBFormat)) = Par2DRGB RGBFormat
    loadResource _ (Par2DRGB fmt) bs = wrapTexResource $ loadTexture' 3 (texture2DFromImage (PerComp3 UnsignedByteFormat) fmt) bs
    saveResource = undefined
instance Resource (TextureResource (Texture2D RGBAFormat)) where
    data ResourceParams (TextureResource (Texture2D RGBAFormat)) = Par2DRGBA RGBAFormat
    loadResource _ (Par2DRGBA fmt) bs = wrapTexResource $ loadTexture' 4 (texture2DFromImage (PerComp4 UnsignedByteFormat) fmt) bs
    saveResource = undefined

instance Resource (TextureResource (Texture1D AlphaFormat)) where
    data ResourceParams (TextureResource (Texture1D AlphaFormat)) = Par1DAlpha AlphaFormat
    loadResource _ (Par1DAlpha fmt) bs = wrapTexResource $ loadTexture' 1 (texture1DFromImage UnsignedByteFormat fmt) bs
    saveResource = undefined
instance Resource (TextureResource (Texture1D LuminanceFormat)) where
    data ResourceParams (TextureResource (Texture1D LuminanceFormat)) = Par1DLuminance LuminanceFormat
    loadResource _ (Par1DLuminance fmt) bs = wrapTexResource $ loadTexture' 1 (texture1DFromImage UnsignedByteFormat fmt) bs
    saveResource = undefined
instance Resource (TextureResource (Texture1D LuminanceAlphaFormat)) where
    data ResourceParams (TextureResource (Texture1D LuminanceAlphaFormat)) = Par1DLuminanceAlpha LuminanceAlphaFormat
    loadResource _ (Par1DLuminanceAlpha fmt) bs = wrapTexResource $ loadTexture' 2 (texture1DFromImage (PerComp2 UnsignedByteFormat) fmt) bs
    saveResource = undefined
instance Resource (TextureResource (Texture1D RGBFormat)) where
    data ResourceParams (TextureResource (Texture1D RGBFormat)) = Par1DRGB RGBFormat
    loadResource _ (Par1DRGB fmt) bs = wrapTexResource $ loadTexture' 3 (texture1DFromImage (PerComp3 UnsignedByteFormat) fmt) bs
    saveResource = undefined
instance Resource (TextureResource (Texture1D RGBAFormat)) where
    data ResourceParams (TextureResource (Texture1D RGBAFormat)) = Par1DRGBA RGBAFormat
    loadResource _ (Par1DRGBA fmt) bs = wrapTexResource $ loadTexture' 4 (texture1DFromImage (PerComp4 UnsignedByteFormat) fmt) bs
    saveResource = undefined

instance Resource (TextureResource (TextureCube AlphaFormat)) where
    data ResourceParams (TextureResource (TextureCube AlphaFormat)) = ParTexCubeAlpha AlphaFormat
    loadResource _ (ParTexCubeAlpha fmt) bs = wrapTexResource $ loadTexture' 1 (textureCubeFromImage UnsignedByteFormat fmt) bs
    saveResource = undefined
instance Resource (TextureResource (TextureCube LuminanceFormat)) where
    data ResourceParams (TextureResource (TextureCube LuminanceFormat)) = ParTexCubeLuminance LuminanceFormat
    loadResource _ (ParTexCubeLuminance fmt) bs = wrapTexResource $ loadTexture' 1 (textureCubeFromImage UnsignedByteFormat fmt) bs
    saveResource = undefined
instance Resource (TextureResource (TextureCube LuminanceAlphaFormat)) where
    data ResourceParams (TextureResource (TextureCube LuminanceAlphaFormat)) = ParTexCubeLuminanceAlpha LuminanceAlphaFormat
    loadResource _ (ParTexCubeLuminanceAlpha fmt) bs = wrapTexResource $ loadTexture' 2 (textureCubeFromImage (PerComp2 UnsignedByteFormat) fmt) bs
    saveResource = undefined
instance Resource (TextureResource (TextureCube RGBFormat)) where
    data ResourceParams (TextureResource (TextureCube RGBFormat)) = ParTexCubeRGB RGBFormat
    loadResource _ (ParTexCubeRGB fmt) bs = wrapTexResource $ loadTexture' 3 (textureCubeFromImage (PerComp3 UnsignedByteFormat) fmt) bs
    saveResource = undefined
instance Resource (TextureResource (TextureCube RGBAFormat)) where
    data ResourceParams (TextureResource (TextureCube RGBAFormat)) = ParTexCubeRGBA RGBAFormat
    loadResource _ (ParTexCubeRGBA fmt) bs = wrapTexResource $ loadTexture' 4 (textureCubeFromImage (PerComp4 UnsignedByteFormat) fmt) bs
    saveResource = undefined

instance Resource (TextureResource3D (Texture3D AlphaFormat)) where
    data ResourceParams (TextureResource3D (Texture3D AlphaFormat)) = ParTex3DAlpha Int AlphaFormat
    loadResource _ (ParTex3DAlpha d fmt) bs = wrapTex3DResource $ loadTexture' 1 (texture3DFromImage' d UnsignedByteFormat fmt) bs
    saveResource = undefined
instance Resource (TextureResource3D (Texture3D LuminanceFormat)) where
    data ResourceParams (TextureResource3D (Texture3D LuminanceFormat)) = ParTex3DLuminance Int LuminanceFormat
    loadResource _ (ParTex3DLuminance d fmt) bs = wrapTex3DResource $ loadTexture' 1 (texture3DFromImage' d UnsignedByteFormat fmt) bs
    saveResource = undefined
instance Resource (TextureResource3D (Texture3D LuminanceAlphaFormat)) where
    data ResourceParams (TextureResource3D (Texture3D LuminanceAlphaFormat)) = ParTex3DLuminanceAlpha Int LuminanceAlphaFormat
    loadResource _ (ParTex3DLuminanceAlpha d fmt) bs = wrapTex3DResource $ loadTexture' 2 (texture3DFromImage' d (PerComp2 UnsignedByteFormat) fmt) bs
    saveResource = undefined
instance Resource (TextureResource3D (Texture3D RGBFormat)) where
    data ResourceParams (TextureResource3D (Texture3D RGBFormat)) = ParTex3DRGB Int RGBFormat
    loadResource _ (ParTex3DRGB d fmt) bs = wrapTex3DResource $ loadTexture' 3 (texture3DFromImage' d (PerComp3 UnsignedByteFormat) fmt) bs
    saveResource = undefined
instance Resource (TextureResource3D (Texture3D RGBAFormat)) where
    data ResourceParams (TextureResource3D (Texture3D RGBAFormat)) = ParTex3DRGBA Int RGBAFormat
    loadResource _ (ParTex3DRGBA d fmt) bs = wrapTex3DResource $ loadTexture' 4 (texture3DFromImage' d (PerComp4 UnsignedByteFormat) fmt) bs
    saveResource = undefined