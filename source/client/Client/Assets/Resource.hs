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
{-# LANGUAGE TypeFamilies #-}
module Client.Assets.Resource(
    Resource(..)
  , loadResource'
  , saveResource'
  ) where

import Data.Typeable
import Control.Monad.Trans.Either
import Data.ByteString.Lazy

addResName :: String -> EitherT String IO a -> EitherT String IO a
addResName name = bimapEitherT (\s -> "Resource name: " ++ name ++ ". " ++ s) id

-- | Safe version of loadResource that adds resource name to the error string.
loadResource' :: (Resource a) => String -> ResourceParams a -> ByteString -> EitherT String IO a
loadResource' name p = addResName name . loadResource name p

-- | Safe version of saveResource that adds resource name to the error string.
saveResource' :: (Resource a) => String -> ResourceParams a -> a -> EitherT String IO ByteString
saveResource' name p = addResName name . saveResource p

class Typeable a => Resource a where
  data ResourceParams a :: *
  loadResource :: String -> ResourceParams a -> ByteString -> EitherT String IO a
  saveResource :: ResourceParams a -> a -> EitherT String IO ByteString