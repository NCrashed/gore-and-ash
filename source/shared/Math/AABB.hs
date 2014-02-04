-- Copyright 2014 Anton Gushcha
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
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Math.AABB(
    AABB(..)
  , newAABB
  , boxMin
  , boxMax
  , extendAABB
  ) where
  
import Math.Ray  
import Math.Collidable
import Data.Vec as V
import Data.Functor
import Data.Monoid
import Control.Arrow

-- | Axis aligned bounding box. Defined as minimum location and maximum location.
data AABB a = AABB
  -- | Box corner with minimal coords.  
  !(Vec3 a)
  -- | Box corner with maximum coords. 
  !(Vec3 a)

-- | Constructs new AABB from to vectors. 
newAABB :: Ord a => Vec3 a -> Vec3 a -> AABB a
newAABB a b 
  | a <= b    = AABB a b
  | otherwise = AABB b a 

-- | Box origin nearest corner.
boxMin :: AABB a -> Vec3 a
boxMin (AABB v _) = v

-- | Box origin farthest corner.
boxMax :: AABB a -> Vec3 a
boxMax (AABB _ v) = v

-- | Creating bounding box that bounds two bounding boxes.  
extendAABB :: Ord a => AABB a -> AABB a -> AABB a
extendAABB (AABB amin amax) (AABB bmin bmax) = AABB cmin cmax
  where
    cmin = if amin <= bmin then amin else bmin
    cmax = if amax >= bmax then amax else bmax

-- | Intersecting axis aligned box with another one. Nothing if
-- boxes are not intersected.
intersectBoxes :: Ord a => AABB a -> AABB a -> Maybe (AABB a)
intersectBoxes (AABB aminv amaxv) (AABB bminv bmaxv) = uncurry AABB <$> unzipProj <$> sequence projs
  where
    projs = V.toList $ V.zipWith intersectProj (V.zipWith (,) aminv amaxv) (V.zipWith (,) bminv bmaxv)
    
    intersectProj :: Ord a => (a, a) -> (a, a) -> Maybe (a, a)
    intersectProj (amin, amax) (bmin, bmax) = if amax < bmin || bmax < amin then Nothing
      else Just (max amin bmin, min amax bmax)
      
    unzipProj :: [(a,a)] -> (Vec3 a, Vec3 a)
    unzipProj =  unzip >>> (V.fromList *** V.fromList)

instance Ord a => Collidable (AABB a) (AABB a) where
  type CollisionResult (AABB a) (AABB a) = AABB a
  intersect = intersectBoxes
  
-- | Intersecting box with a ray. Returns Nothing if ray doesn't touch box and
-- | returns a pair of ray parameters for in and out point of ray way through the box.
intersectBoxAndRay :: (Floating a, Ord a) => AABB a -> Ray a -> Maybe (a, a) 
intersectBoxAndRay (AABB (minBoxX:.minBoxY:.minBoxZ:.()) (maxBoxX:.maxBoxY:.maxBoxZ:.())) (Ray (originX:.originY:.originZ:.()) (dirX:.dirY:.dirZ:.())) 
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
    
instance (Floating a, Ord a) => Collidable (AABB a) (Ray a) where
  type CollisionResult (AABB a) (Ray a) = (a, a)
  intersect = intersectBoxAndRay
 
instance (Floating a, Ord a) => Collidable (Ray a) (AABB a) where
  type CollisionResult (Ray a) (AABB a) = (a, a)
  intersect = flip intersectBoxAndRay 
  
instance (Ord a, Num a) => Monoid (AABB a) where
  mempty = AABB (vec 0) (vec 0)
  mappend = extendAABB