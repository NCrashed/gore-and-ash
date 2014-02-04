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
module Game.LocatedObject(
    Location
  , Rotation
  , LocatedObject(..)
  )where
  
import Data.Vec
import Math.Quaternion
  
type Location = Vec3 Float
type Rotation = Quaternion Float

-- | Class describing objects that can be located in "World". 
-- They have location and rotation, also class defines functions
-- to translate local to global coordinate system and vice versa.
--
-- Minimum defenition: all except toLocal and toGlobal
--
-- Coordinate system functions should satisfy following equations:
--
-- @ forall v . v == toGlobal . toLocal v
-- @ forall v . toLocal . toGlobal v == v
class LocatedObject a where
  getLocation :: a -> Location
  getRotation :: a -> Rotation
  setLocation :: a -> Location -> a
  setRotation :: a -> Rotation ->  a
  toLocal  :: a -> Location -> Location
  toGlobal :: a -> Location -> Location
  
  toLocal  o = (+) (getLocation o) . rotateVec (getRotation o)
  toGlobal o = rotateVec (quatInverse (getRotation o)) . (-) (getLocation o)