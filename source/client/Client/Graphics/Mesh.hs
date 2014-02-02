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
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Client.Graphics.Mesh(
    Mesh
  , MeshContainer(..)
  , renderMesh
  ) where

import Prelude as P  
import Client.Graphics.GPipe
import Data.Sequence as S
import Data.Foldable
import Data.Monoid

type Mesh = MeshContainer (Vec3 Float, Vec3 Float, Vec2 Float)
data MeshContainer a = MeshContainer Triangle (Seq a)

renderMesh :: Mesh -> PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
renderMesh (MeshContainer tr vecs) = toGPUStream tr $ list
  where
    list = toList vecs

instance Functor MeshContainer where
  fmap f (MeshContainer tr d) = MeshContainer tr (fmap f d)
  
instance Monoid Mesh where
  mempty = MeshContainer TriangleList empty
  mappend (MeshContainer ra av) (MeshContainer rb bv)
    | ra == rb  = MeshContainer ra (mappend av bv) 
    | otherwise = MeshContainer TriangleList (mappend (toTriangleList ra av) (toTriangleList rb bv))
  
toTriangleList :: Triangle -> Seq (Vec3 Float, Vec3 Float, Vec2 Float) -> Seq (Vec3 Float, Vec3 Float, Vec2 Float)
toTriangleList TriangleList  = id
toTriangleList TriangleStrip = fromList . P.concat . fromStrip 0 [] . toList
  where 
    fromStrip :: Int -> [[a]] -> [a] -> [[a]]
    fromStrip i acc (x:y:z:xs) 
      | i `mod` 2 == 0 = fromStrip (i+1) ([z,x,y]:acc) (y:z:xs)
      | otherwise      = fromStrip (i+1) ([z,y,x]:acc) (y:z:xs)
    fromStrip _ acc _ = acc
toTriangleList TriangleFan = \xs -> case toList xs of
  [] -> S.empty
  ls -> fromList $ P.concat $ fromFan (head ls) [] (tail ls)
  where
    fromFan :: a -> [[a]] -> [a] -> [[a]]
    fromFan c acc (x:y:xs) = fromFan c ([y,x,c]:acc) (y:xs)
    fromFan _ acc _ = acc