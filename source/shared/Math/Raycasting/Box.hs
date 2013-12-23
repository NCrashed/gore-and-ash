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
module Math.Raycasting.Box(
    Box(..)
  , newBox
  , boxMin
  , boxMax
  , intersectBoxAndRay
  ) where
  
import Data.Vec
import Math.Raycasting.Ray

-- | Axis aligned box form min point and max point.
data Box = Box
  -- | Box corner with minimal coords. 
  !(Vec3 Float) 
  -- | Box corner with maximum coords.
  !(Vec3 Float)

-- | Constructing new box with checking corners relation (switches them if necessary).
newBox :: Vec3 Float -> Vec3 Float -> Box
newBox a b = if a > b then Box b a else Box a b

-- | Box origin nearest corner.
boxMin :: Box -> Vec3 Float
boxMin (Box v _) = v

-- | Box origin farthest corner.
boxMax :: Box -> Vec3 Float
boxMax (Box _ v) = v

-- | Intersecting box with a ray. Returns Nothing if ray doesn't touch box and
-- | returns a pair of ray parameters for in and out point of ray way through the box.
intersectBoxAndRay :: Box -> Ray -> Maybe (Float, Float) 
intersectBoxAndRay (Box (minBoxX:.minBoxY:.minBoxZ:.()) (maxBoxX:.maxBoxY:.maxBoxZ:.())) (Ray (originX:.originY:.originZ:.()) (dirX:.dirY:.dirZ:.())) 
  | (tminX > tmaxY || tminY > tmaxX) || (tminXY > tmaxZ || tminZ > tmaxXY) = Nothing
  | otherwise = Just (tminXYZ, tmaxXYZ)
  where
    divX = 1 / dirX
    tminX = if divX >= 0 then (minBoxX - originX) * divX else (maxBoxX - originX) * divX
    tmaxX = if divX  < 0 then (maxBoxX - originX) * divX else (minBoxX - originX) * divX
    divY = 1 / dirY
    tminY = if divY >= 0 then (minBoxY - originY) * divY else (maxBoxY - originY) * divY
    tmaxY = if divY  < 0 then (maxBoxY - originY) * divY else (minBoxY - originY) * divY
    divZ = 1 / dirZ
    tminZ = if divZ >= 0 then (minBoxZ - originZ) * divZ else (maxBoxZ - originZ) * divZ
    tmaxZ = if divZ  < 0 then (maxBoxZ - originZ) * divZ else (minBoxZ - originZ) * divZ
    
    tminXY = if tminY > tminX then tminY else tminX
    tmaxXY = if tmaxY < tmaxX then tmaxY else tmaxX  
    tminXYZ = if tminZ > tminXY then tminZ else tminXY
    tmaxXYZ = if tmaxZ < tmaxXY then tmaxX else tmaxXY
    