{-# LANGUAGE DoAndIfThenElse #-}
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
import Graphics.UI.GLUT(
      Window
    , mainLoop
    , postRedisplay
    , idleCallback
    , getArgsAndInitialize
    , ($=))

import Client.Graphics.Boxed.Chunk
import Game.Boxed.Chunk
import Game.Boxed.BlockManager
import Game.Boxed.SimpleBlock
import Data.Maybe (fromJust)
import Client.Assets.Manager
import System.FilePath
import Control.Monad.Trans.Either

initGraphicsSystem :: ProcessId -> Process ProcessId
initGraphicsSystem _ = spawnLocal $ liftIO $ do
    _ <- getArgsAndInitialize
   
    renderFunc <- prepareChunkFrame
    newWindow "Gore & Ash" (100:.100:.()) (800:.600:.()) renderFunc initWindow
    mainLoop

prepareChunkFrame :: IO (Vec2 Int -> IO (FrameBuffer RGBAFormat DepthFormat ()))
prepareChunkFrame = do
  resMng <- liftIO $ addNewFileSystemPack emptyResourceManager "test" ("media" </> "test")
  angleRef <- newIORef 0.0
  let Right blockMng = registerBlocks
  Right (chunk, _) <- runEitherT $ buildChunkModel (fromJust $ calcChunk blockMng) resMng
  return $ renderChunkFrame  angleRef chunk
  where
    calcChunk mng = chunkFromList 4 mng [z, f, z, s, z, f, z, s, z, f, z, s, s, z, s, z
                                        ,f, s, f, z, z, z, f, f, s, s, z, z, z, f, z, s
                                        ,z, z, f, z, f, z, z, s, f, s, z, f, z, s, f, s
                                        ,z, f, s, s, z, s, s, s, z, z, s, z, s, z, z, s]
      where
      f = "Plating"  
      s = "Metal" 
      z = blockName SpaceBlock                               
    registerBlocks = do
      mng <- registerBlock emptyBlockManager $ uniformBlock "Plating" "test:1S03.png"
      registerBlock mng $ uniformBlock "Metal" "test:2S03.png"

renderChunkFrame :: IORef Float -> ChunkModel -> Vec2 Int -> IO (FrameBuffer RGBAFormat DepthFormat ())
renderChunkFrame angleRef chunk size = do
    angle <- readIORef angleRef
    writeIORef angleRef ((angle + 0.005) `mod'` (2*pi))
    return $ chunkFrameBuffer chunk angle size
         
initWindow :: Window -> IO ()
initWindow win = idleCallback $= Just (postRedisplay (Just win) >> yield)