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
{-# LANGUAGE ScopedTypeVariables #-}
module Util.Monad(
    liftExceptions
  ) where
  
import Control.Exception
import Control.Monad.Trans (liftIO)  
import Control.Monad.Trans.Either

-- | Catches all synchronous exceptions all transforms them into EitherT String
-- | monad transformer. 
liftExceptions :: forall a . IO a -> EitherT String IO a
liftExceptions action = do
  res <- liftIO (try action :: IO (Either SomeException a))
  eitherT (left.show) right $ hoistEither res