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
module Client.Graphics.GPipe.Inner.FrameBufferObject(
    FrameBufferObject()
  ) where
  
import Client.Graphics.GPipe.Inner.Resources
import Client.Graphics.GPipe.Inner.Formats
import Client.Graphics.GPipe.Inner.StrictIO
--import Graphics.Rendering.OpenGL

data FrameBufferObject c d s = FrameBufferObject (ContextCacheIO ())

newFBOColor :: ColorFormat f =>  Color f Float -> FrameBufferObject f () ()
newFBOColor c = undefined--FrameBufferObject $ do
  

--newFrameBufferColor c = FrameBuffer $ do
--    c' <- ioEvaluateColor 0 1 c
--    setContextWindow
--    liftIO $ do setDefaultStates
--                setNewColor c'
--                clear [GL.ColorBuffer]