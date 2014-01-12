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
module Client.Graphics.GPipe.Inner.StrictIO(
    ioEvaluate
  , ioEvaluateColor
  , evaluateDeep
  , evaluatePtr
  ) where
  
import Client.Graphics.GPipe.Inner.Formats
import Client.Graphics.GPipe.Inner.Resources
import Control.Exception (evaluate)
import Data.Vec
import Foreign.Ptr
import Foreign.C.Types (CUChar)
import Foreign.Storable (peek)

evaluateDeep :: Eq b => b -> IO b
evaluateDeep a = evaluate (a==a) >> return a

ioEvaluate :: Eq a => a -> ContextCacheIO a
ioEvaluate = liftIO . evaluateDeep

evaluatePtr :: Ptr a -> IO (Ptr a)
evaluatePtr p = do a <- peek (castPtr p :: Ptr CUChar)
                   evaluate (a==a) >> return p
                   
ioEvaluateColor :: (Eq a, ColorFormat f) => a -> a -> Color f a -> ContextCacheIO (Vec4 a)
ioEvaluateColor z e x = let (a:.b:.c:.d:.()) = fromColor z e x
                        in do a' <- ioEvaluate a
                              b' <- ioEvaluate b
                              c' <- ioEvaluate c
                              d' <- ioEvaluate d
                              return (a':.b':.c':.d':.())