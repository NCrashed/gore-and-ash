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

import Client.Graphics.GPipe
import Data.IORef
import Graphics.UI.GLUT(
      Window
    , mainLoop
    , postRedisplay
    , idleCallback
    , getArgsAndInitialize
    , ($=))

import Client.Graphics.Boxed.Chunk
import Game.Boxed.Chunk
import Data.Maybe (fromJust)

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
    return $ chunkFrameBuffer undefined (fromJust $ calcChunk (angle > pi)) angle size
    where
        calcChunk flag = chunkFromList 4   [z, c, z, c, z, c, z, c, z, c, z, c, c, z, c, z
                                           ,c, c, c, z, z, z, c, c, c, c, z, z, z, c, z, c
                                           ,z, z, c, z, c, z, z, c, c, c, z, c, z, c, c, c
                                           ,z, c, c, c, z, c, c, c, z, z, c, z, c, z, z, c]
          where                                           
            c = if flag then 1 else 0
            z = if flag then 0 else 1
    
initWindow :: Window -> IO ()
initWindow win = idleCallback $= Just (postRedisplay (Just win) >> yield)