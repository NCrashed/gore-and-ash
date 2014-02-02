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
{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Util.Convert(
    ConvertToRepa(..)
  , ConvertToVec(..)
  ) where
  
import qualified Data.Array.Repa as R
import qualified Data.Vec        as V

-- | Provides ability to convert vectors to repa shapes.
class ConvertToRepa a where
  type RepaShape a
  toShape :: a -> RepaShape a

-- | Provides ability to convert repa shapes to vectors.
class ConvertToVec a where
  type VecType a
  toVec :: a -> VecType a
  
instance ConvertToRepa () where
  type RepaShape () = R.Z
  toShape = const R.Z
  
instance ConvertToVec R.Z where
  type VecType R.Z = ()
  toVec = const ()
  
instance (ConvertToRepa b) => ConvertToRepa (a V.:. b) where
  type RepaShape (a V.:. b) = RepaShape b R.:. a  
  toShape (a V.:. b) = toShape b R.:. a

instance (ConvertToVec a) => ConvertToVec (a R.:. b) where
  type VecType (a R.:. b) = b V.:. VecType a
  toVec (a R.:. b) = b V.:. toVec a