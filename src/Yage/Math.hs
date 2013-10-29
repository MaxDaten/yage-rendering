{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Yage.Math
    (fromTransformation
    , (><)
    , normal, normals
    , uv00, uv01, uv10, uv11, xAxis, yAxis, zAxis
    , plainNormalForm) where

import Yage.Prelude
import Control.Lens
import Data.List (map)
import Linear (V2(..), V3(..), V4(..), M33, M44, cross, normalize, Epsilon)
import Yage.Rendering.Types

zero, one :: (Floating a) => a
zero = 0.0
one  = 1.0

uv00, uv01, uv10, uv11 :: (Floating a) => V2 a
uv00  = V2 zero zero 
uv01  = V2 zero one
uv10  = V2 one zero
uv11  = V2 one one

xAxis, yAxis, zAxis :: (Floating a) => V3 a
xAxis = V3 one zero zero
yAxis = V3 zero one zero
zAxis = V3 zero zero one

type Normal a = V3 a
-- | a plain in 3d space in plain normal form 
type Plain3DNF a = (Normal a, V3 a, V3 a)

infixl 7 ><

(><):: Num a => V3 a -> V3 a -> V3 a
(><) = cross

fromTransformation :: M44 a -> M33 a
fromTransformation
    (V4 (V4 a b c _)
        (V4 d e f _)
        (V4 g h i _)
        (V4 _ _ _ _)) = V3 (V3 a b c)
                           (V3 d e f)
                           (V3 g h i)

-- | calculates a normal from two vectors in local space (no position vectors)
normal :: (Num a, Floating a, Epsilon a) => V3 a -> V3 a -> V3 a
normal v1 v2 = normalize $ cross v1 v2


-- | calculates a plain in normal form from three position vectors
plainNormalForm :: (Num a, Floating a, Epsilon a) => V3 a -> V3 a -> V3 a -> Plain3DNF a
plainNormalForm v1 v2 v3 = 
    let n = normal (v2 - v1) (v2 - v3)
        p = v1
        q = v2
    in (n, p, q)

-- possible not the fastest implementation
normals :: (Num a, Floating a, Epsilon a) => [V3 a] -> [V3 a]
normals vs = map norms $ splitEvery 3 vs
    where
        norms (a:b:c:[]) = (plainNormalForm a b c)^._1

{--
genNormals :: (Num a, Show a, Floating a) => [V3 a] -> [V3 a]
genNormals vs =
    let ns = concat $ map (\(a:b:_) -> replicate 3 $ a `normal` b) $ splitEvery 3 vs
        cs = repeat color
    in zipWith3 Vertex vs ns cs
--}