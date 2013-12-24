-----------------------------------------------------------------------------
--
-- Module      :  GPipe
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- GPipe models the entire graphics pipeline in a purely functional, immutable and typesafe way. It is built on top of the programmable pipeline (i.e. non-fixed function) of
-- OpenGL 2.1 and uses features such as vertex buffer objects (VBO's), texture objects and GLSL shader code synthetisation to create fast graphics programs. Buffers,
-- textures and shaders are cached internally to ensure fast framerate, and GPipe is also capable of managing multiple windows and contexts. By creating your own
-- instances of GPipes classes, it's possible to use additional datatypes on the GPU.
--
-- You'll need full OpenGL 2.1 support, including GLSL 1.20 to use GPipe. Thanks to OpenGLRaw, you may still build GPipe programs on machines lacking this support.
--
-- This is a conveniance module, combining GPipes all other modules.
-----------------------------------------------------------------------------

module Client.Graphics.GPipe (
    module X
) where

import Client.Graphics.GPipe.Stream as X
import Client.Graphics.GPipe.Stream.Primitive as X
import Client.Graphics.GPipe.Stream.Fragment as X
import Client.Graphics.GPipe.FrameBuffer as X
import Client.Graphics.GPipe.Texture as X
import Client.Graphics.GPipe.Format as X
