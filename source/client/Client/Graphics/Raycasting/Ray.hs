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
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Client.Graphics.Raycasting.Ray(
    GPURay(..)
  , rayPoint
  ) where
  
import Client.Graphics.GPipe
import Data.Vec as Vec

data GPURay a = GPURay !(Vec3 a) !(Vec3 a)
 
instance GPU a => GPU (GPURay a) where 
  type CPU (GPURay a) = GPURay (CPU a)
  toGPU (GPURay a b)= GPURay (toGPU a) (toGPU b)

rayPoint :: GPURay (Fragment Float) -> Fragment Float -> Vec3 (Fragment Float)
rayPoint (GPURay origin direction) t = origin + (Vec.map (*t) direction)