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
module Client.Graphics.System(
      initGraphicsSystem
    ) where
    
import Control.Distributed.Process
import Control.Concurrent (yield)

import Graphics.GPipe
import Data.IORef
import Data.Word
import Data.Bits
import Graphics.UI.GLUT(
      Window
    , mainLoop
    , postRedisplay
    , idleCallback
    , getArgsAndInitialize
    , ($=))

import Client.Graphics.Voxel.Chunk
import Game.Boxed.Chunk

initGraphicsSystem :: ProcessId -> Process ProcessId
initGraphicsSystem _ = spawnLocal $ liftIO $ do
    _ <- getArgsAndInitialize
    angleRef <- newIORef 0.0
    newWindow "Test window" (100:.100:.()) (800:.600:.()) (renderFrame angleRef) initWindow
    mainLoop
    
renderFrame :: IORef Float -> Vec2 Int -> IO (FrameBuffer RGBFormat DepthFormat ())
renderFrame angleRef size = do
    angle <- readIORef angleRef
    writeIORef angleRef ((angle + 0.005) `mod'` (2*pi))
    return $ chunkFrameBuffer chunk angle size
    where
        Just chunk = chunkFromList 4   [z, c, z, c, z, c, z, c, z, c, z, c, c, z, c, z
                                       ,c, c, c, z, z, z, c, c, c, c, z, z, z, c, z, c
                                       ,z, z, c, z, c, z, z, c, c, c, z, c, z, c, c, c
                                       ,z, c, c, c, z, c, c, c, z, z, c, z, c, z, z, c]
        c = rgba2word32 $ RGBA (0:. 115 :. 255:.()) 0
        z = rgba2word32 $ RGBA (0:.0:.0:.()) 255                                    

rgba2word32 :: Color RGBAFormat Word8 -> Word32
rgba2word32 (RGBA (red:.green:.blue:.()) alpha) = red' .|. green' .|. blue' .|. alpha'
  where
    alpha' = fromIntegral alpha
    blue'  = shift (fromIntegral blue)   8
    green' = shift (fromIntegral green) 16
    red'   = shift (fromIntegral red)   24
    
initWindow :: Window -> IO ()
initWindow win = idleCallback $= Just (postRedisplay (Just win) >> yield)