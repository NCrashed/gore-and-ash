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
module Math.Raycasting.Ray(
    Ray(..)
  , newRay
  , rayOrigin
  , rayDirection
  ) where
  
import Data.Vec

-- | Ray is a origin and direction, floats used as they passed to gpu with
-- | minimum payload. 
data Ray = Ray 
  -- | Ray origin
  !(Vec3 Float) 
  -- | Ray direction, always normalized
  !(Vec3 Float)
  
-- | Constructing new ray, used for automatic direction normalization.
newRay :: Vec3 Float -> Vec3 Float -> Ray
newRay origin dir = Ray origin (normalize dir)

-- | Returs ray origin vector
rayOrigin :: Ray -> Vec3 Float
rayOrigin (Ray origin _ ) = origin

-- | Returns ray direction vector
rayDirection :: Ray -> Vec3 Float
rayDirection (Ray _ direction) = direction