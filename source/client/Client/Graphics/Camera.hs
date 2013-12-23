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
module Client.Graphics.Camera(
    Camera
  , newCamera
  , cameraMatrix
  ) where
  
import Data.Vec

data Camera = Camera
  -- | Camera Eye position
  !(Vec3 Float)
  -- | Camera Forward direction
  !(Vec3 Float)
  -- | Camera Up direction
  !(Vec3 Float)


newCamera :: Vec3 Float -> Vec3 Float -> Vec3 Float -> Camera
newCamera eye forward up = Camera eye (normalize forward) (normalize up)

cameraMatrix :: Camera -> Mat44 Float
cameraMatrix (Camera eye forward up) = rowX :. rowY :. rowZ :. rowW :. ()
  where
    xaxis = up `cross` zaxis
    yaxis = up
    zaxis = forward
    rowX = xaxis `append` ((-(xaxis `dot` eye)):.())
    rowY = yaxis `append` ((-(yaxis `dot` eye)):.())
    rowZ = zaxis `append` ((-(zaxis `dot` eye)):.())
    rowW :: Vec4 Float
    rowW = 0.0 :. 0.0 :. 0.0 :. 1.0 :. ()