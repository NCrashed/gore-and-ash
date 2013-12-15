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

import Paths_GoreAndAsh 

import Graphics.GPipe
import Graphics.GPipe.Texture.Load
import Data.IORef
import Graphics.UI.GLUT(
      Window
    , mainLoop
    , postRedisplay
    , idleCallback
    , getArgsAndInitialize
    , ($=))

import Client.Graphics.PolyCube

initGraphicsSystem :: ProcessId -> Process ProcessId
initGraphicsSystem _ = spawnLocal $ liftIO $ do
    _ <- getArgsAndInitialize
    tex <- loadTexture RGB8 =<< getDataFileName "assets/textures/planks.jpg"
    angleRef <- newIORef 0.0
    newWindow "Test window" (100:.100:.()) (800:.600:.()) (renderFrame tex angleRef) initWindow
    mainLoop
    
renderFrame :: Texture2D RGBFormat -> IORef Float -> Vec2 Int -> IO (FrameBuffer RGBFormat () ())
renderFrame tex angleRef size = do
    angle <- readIORef angleRef
    writeIORef angleRef ((angle + 0.005) `mod'` (2*pi))
    return $ cubeFrameBuffer tex angle size
    
initWindow :: Window -> IO ()
initWindow win = idleCallback $= Just (postRedisplay (Just win) >> yield)