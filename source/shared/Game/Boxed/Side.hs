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
{-# LANGUAGE DeriveDataTypeable #-}
module Game.Boxed.Side(
    Side(..)
  , sideDirection
  , directionSide
  ) where

import Data.Typeable
import Data.Vec as Vec

data Side = Upward | Downward | Forward | Backward | Leftward | Rightward | Unknownward
  deriving (Typeable, Eq, Ord, Enum)
  
sideDirection :: Side -> Vec3 Int
sideDirection Upward      = 0    :. 1    :. 0    :. ()
sideDirection Downward    = 0    :. (-1) :. 0    :. ()
sideDirection Forward     = 1    :. 0    :. 0    :. ()
sideDirection Backward    = (-1) :. 0    :. 0    :. ()
sideDirection Rightward   = 0    :. 0    :. 1    :. ()
sideDirection Leftward    = 0    :. 0    :. (-1) :. ()
sideDirection Unknownward = 0    :. 0    :. 0    :. ()

directionSide :: Vec3 Int -> Side
directionSide = directionSide' . Vec.map signum
  where
    directionSide' (0    :. 1    :. 0    :. ()) = Upward
    directionSide' (0    :. (-1) :. 0    :. ()) = Downward
    directionSide' (1    :. 0    :. 0    :. ()) = Forward
    directionSide' ((-1) :. 0    :. 0    :. ()) = Backward
    directionSide' (0    :. 0    :. 1    :. ()) = Rightward
    directionSide' (0    :. 0    :. (-1) :. ()) = Leftward
    directionSide' _ = Unknownward
    