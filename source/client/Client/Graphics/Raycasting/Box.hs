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
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies #-}
module Client.Graphics.Raycasting.Box(
    GPUBox(..)
  , intersectBoxAndRay
  ) where
  
import Graphics.GPipe
import Client.Graphics.Raycasting.Ray

-- | Axis aligned box
data GPUBox a = GPUBox
  -- | Box corner nearest to the origin
  !(Vec3 a)
  -- | Box corner fathest from the origin
  !(Vec3 a) 
  
instance GPU a => GPU (GPUBox a) where
  type CPU (GPUBox a) = GPUBox (CPU a)
  toGPU (GPUBox a b) = GPUBox (toGPU a) (toGPU b)
  
-- | Intersecting box with a ray. Returns Nothing if ray doesn't touch box and
-- | returns a pair of ray parameters for in and out point of ray way through the box.
intersectBoxAndRay :: GPUBox (Fragment Float) -> GPURay (Fragment Float)  -> (Fragment Bool, Fragment Float, Fragment Float)
intersectBoxAndRay (GPUBox (minBoxX:.minBoxY:.minBoxZ:.()) (maxBoxX:.maxBoxY:.maxBoxZ:.())) (GPURay (originX:.originY:.originZ:.()) (dirX:.dirY:.dirZ:.())) 
  = ifB ((tminX >* tmaxY ||* tminY >* tmaxX) ||* (tminXY >* tmaxZ ||* tminZ >* tmaxXY)) (false, 0, 0) (true, tminXYZ, tmaxXYZ) 
  where
    divX = 1 / dirX
    tminX = ifB (divX >=* 0) ((minBoxX - originX) * divX) ((maxBoxX - originX) * divX)
    tmaxX = ifB (divX >=* 0) ((maxBoxX - originX) * divX) ((minBoxX - originX) * divX)
   
    divY = 1 / dirY
    tminY = ifB (divY >=* 0) ((minBoxY - originY) * divY) ((maxBoxY - originY) * divY)
    tmaxY = ifB (divY >=* 0) ((maxBoxY - originY) * divY) ((minBoxY - originY) * divY)
    
    divZ = 1 / dirZ
    tminZ = ifB (divZ >=* 0) ((minBoxZ - originZ) * divZ) ((maxBoxZ - originZ) * divZ)
    tmaxZ = ifB (divZ >=* 0) ((maxBoxZ - originZ) * divZ) ((minBoxZ - originZ) * divZ)
    
    tminXY = ifB (tminY >* tminX) tminY tminX
    tmaxXY = ifB (tmaxY <* tmaxX) tmaxY tmaxX  
    tminXYZ = ifB (tminZ >* tminXY) tminZ tminXY
    tmaxXYZ = ifB (tmaxZ <* tmaxXY) tmaxZ tmaxXY
     
