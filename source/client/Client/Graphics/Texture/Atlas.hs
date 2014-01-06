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
{-# LANGUAGE ExistentialQuantification #-}
module Client.Graphics.Texture.Atlas(
    Atlas()
  ) where
  
import Client.Graphics.GPipe
import Data.HashMap
import Client.Assets.Manager
import Control.Monad.Trans.Either

data Atlas = forall f . (ColorFormat f) => Atlas LookupTable (Texture2D f)

type LookupTable = Map String (Vec2 Int)

emptyAtlas :: Atlas
emptyAtlas = undefined

atlasShape :: Atlas -> Vec2 Int
atlasShape = undefined

updateAtlas :: ResourceManager -> [String] -> EitherT String IO Atlas
updateAtlas = undefined

removeFromAtlas :: Atlas -> [String] -> Atlas
removeFromAtlas = undefined

renderAtlas :: ColorFormat f => Atlas -> FrameBuffer f () ()
renderAtlas = undefined

-- also instance for resource for atlas