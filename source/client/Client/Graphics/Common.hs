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
module Client.Graphics.Common(
      paintSolid
    , paintSolidAlpha
    , paintSolidDepth
    , paintSolidDepthAlpha
    , emptyFrameBuffer
    , emptyFrameBufferAlpha 
    , emptyFrameBufferDepth
    , emptyFrameBufferDepthAlpha
    , enlight
    , transform
    ) where
    
import Graphics.GPipe
import Data.Vec as Vec
    
paintSolid :: FragmentStream (Color RGBFormat (Fragment Float)) -> FrameBuffer RGBFormat () () -> FrameBuffer RGBFormat () ()
paintSolid = paintColor NoBlending (RGB $ vec True)

paintSolidAlpha :: FragmentStream (Color RGBAFormat (Fragment Float)) -> FrameBuffer RGBAFormat () () -> FrameBuffer RGBAFormat () ()
paintSolidAlpha = paintColor NoBlending (RGBA (vec True) True)

paintSolidDepth :: FragmentStream (Color RGBFormat (Fragment Float), FragmentDepth) -> FrameBuffer RGBFormat DepthFormat () -> FrameBuffer RGBFormat DepthFormat ()
paintSolidDepth = paintColorDepth Less True NoBlending (RGB $ vec True)

paintSolidDepthAlpha :: FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth) -> FrameBuffer RGBAFormat DepthFormat () -> FrameBuffer RGBAFormat DepthFormat ()
paintSolidDepthAlpha = paintColorDepth Less True NoBlending (RGBA (vec True) True)

emptyFrameBuffer :: FrameBuffer RGBFormat () ()
emptyFrameBuffer = newFrameBufferColor (RGB 0)

emptyFrameBufferAlpha :: FrameBuffer RGBAFormat () () 
emptyFrameBufferAlpha = newFrameBufferColor (RGBA 0 1)

emptyFrameBufferDepth :: FrameBuffer RGBFormat DepthFormat ()
emptyFrameBufferDepth = newFrameBufferColorDepth (RGB 0) 100
  
emptyFrameBufferDepthAlpha :: FrameBuffer RGBAFormat DepthFormat ()
emptyFrameBufferDepthAlpha = newFrameBufferColorDepth (RGBA 0 1) 100
    
enlight :: Texture2D RGBAFormat -> (Vec3 (Fragment Float), Vec2 (Fragment Float), FragmentDepth) -> (Color RGBAFormat (Fragment Float), FragmentDepth)
enlight tex (normv, uv, depth) = (RGBA (c * vec (normv `dot` toGPU (0:.0.45:.1:.()))) a, depth)
    where RGBA c a = sample (Sampler Linear Wrap) tex uv
    
transform :: Float -> Vec2 Int -> (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float)) -> (Vec4 (Vertex Float), (Vec3 (Vertex Float), Vec2 (Vertex Float)))
transform angle (width:.height:.()) (pos, normv, uv) = (transformedPos, (transformedNorm, uv))
    where
    modelMat = rotationVec (normalize (1:.0.5:.0.3:.())) angle `multmm` translation (-0.5)
    viewMat  = translation (-(0:.0:.2:.()))
    projMat  = perspective 1 100 (pi/3) (fromIntegral width / fromIntegral height)
    viewProjMat = projMat `multmm` viewMat
    transformedPos  = toGPU (viewProjMat `multmm` modelMat) `multmv` (homPoint pos :: Vec4 (Vertex Float))
    transformedNorm = toGPU (mat4ToMat3 modelMat) `multmv` normv
    mat4ToMat3 = Vec.map (Vec.take n3) . Vec.take n3    