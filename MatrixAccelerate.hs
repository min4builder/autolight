{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, RebindableSyntax, TypeFamilies #-}
module MatrixAccelerate (
    Matrix(..), MResult, MNormal, Shape,
    Dim1, Dim2, Dim3, Dim4, Dim5,
    zero, toIndex, fromIndex, toLength, sinside, off,
    mindex, minside, mmap, mresult, mrun, msize, mzipWith,
    Elt, Exp, Bool(..), Int, Float,
    fromInteger, fromIntegral, fromRational, negate, ifThenElse, round,
    exp, sqrt, pi, minimum, sum, signum,
    lift, unlift, constant,
    (==), (/=), ($), (+), (-), (*), (/), (**), (<), (>), (.), (&&), (||),
) where

import Data.Array.Accelerate hiding (Matrix, Shape, tail, toIndex, fromIndex, minimum, size, sum)
import qualified Data.Array.Accelerate as A
import Prelude (head, tail, id)
import qualified Prelude

class Shape sh where
    zero :: sh
    toIndex :: sh -> sh -> Exp Int
    fromIndex :: sh -> Exp Int -> sh
    toLength :: sh -> Exp Int
    sinside :: sh -> sh -> Exp Bool
    off :: sh -> sh -> sh

type Dim1 = Exp Int
instance Shape (Exp Int) where
    zero = 0
    toIndex _ x = x
    fromIndex _ s = s
    toLength x = x
    sinside s x = 0 <= x && x < s
    off = (+)
type Dim2 = (Exp Int, Exp Int)
instance Shape (Exp Int, Exp Int) where
    zero = (0, 0)
    toIndex (s, _) (x, y) = x + s * y
    fromIndex (x, _) s = (s `mod` x, s `div` x)
    toLength (x, y) = x * y
    sinside (s, t) (x, y) = 0 <= x && x < s && 0 <= y && y < t
    off (s, t) (x, y) = (s + x, t + y)
type Dim3 = (Exp Int, Exp Int, Exp Int)
instance Shape (Exp Int, Exp Int, Exp Int) where
    zero = (0, 0, 0)
    toIndex (s, t, _) (x, y, z) = x + s * y + s * t * z
    fromIndex (x, y, _) s = (s `mod` x, s `div` x `mod` y, s `div` x `div` y)
    toLength (x, y, z) = x * y * z
    sinside (s, t, u) (x, y, z) = 0 <= x && x < s && 0 <= y && y < t && 0 <= z && z < u
    off (s, t, u) (x, y, z) = (s + x, t + y, u + z)
type Dim4 = (Exp Int, Exp Int, Exp Int, Exp Int)
instance Shape (Exp Int, Exp Int, Exp Int, Exp Int) where
    zero = (0, 0, 0, 0)
    toIndex (s, t, u, _) (x, y, z, w) = x + s * y + s * t * z + s * t * u * w
    fromIndex (x, y, z, _) s = (s `mod` x, s `div` x `mod` y, s `div` x `div` y `mod` z, s `div` x `div` y `div` z)
    toLength (x, y, z, w) = x * y * z * w
    sinside (s, t, u, v) (x, y, z, w) = 0 <= x && x < s && 0 <= y && y < t && 0 <= z && z < u && 0 <= w && w < v
    off (s, t, u, v) (x, y, z, w) = (s + x, t + y, u + z, v + w)
type Dim5 = (Exp Int, Exp Int, Exp Int, Exp Int, Exp Int)
instance Shape (Exp Int, Exp Int, Exp Int, Exp Int, Exp Int) where
    zero = (0, 0, 0, 0, 0)
    toIndex (s, t, u, v, _) (x, y, z, w, ww) = x + s * y + s * t * z + s * t * u * w + s * t * u * v * ww
    fromIndex (x, y, z, w, _) s = (s `mod` x, s `div` x `mod` y, s `div` x `div` y `mod` z, s `div` x `div` y `div` z `mod` w, s `div` x `div` y `div` z `div` w)
    toLength (x, y, z, w, ww) = x * y * z * w * ww
    sinside (s, t, u, v, q) (x, y, z, w, ww) = 0 <= x && x < s && 0 <= y && y < t && 0 <= z && z < u && 0 <= w && w < v && 0 <= ww && ww < q
    off (s, t, u, v, q) (x, y, z, w, ww) = (s + x, t + y, u + z, v + w, q + ww)

minimum l = minimum' (head l) (tail l)
    where minimum' e [] = e
          minimum' e (a:r) = if a < e then minimum' a r else minimum' e r

sum [] = 0
sum (a:r) = a + sum r

data Matrix r sh a = Matrix sh (Acc (Array DIM1 a))
type family MResult (m :: * -> * -> * -> *) :: *
type instance MResult Matrix = ()
type family MNormal (m :: * -> * -> * -> *) :: *
type instance MNormal Matrix = ()

mindex :: (Elt a, Shape sh) => Matrix r sh a -> sh -> Exp a
mindex (Matrix sh v) p = v ! lift (Z :. toIndex sh p)
{-# INLINEABLE mindex #-}

minside :: Shape sh => Matrix r sh a -> sh -> Exp Bool
minside v = sinside (msize v)
{-# INLINEABLE minside #-}

mmap :: (Elt a, Elt b) => (Exp a -> Exp b) -> Matrix r sh a -> Matrix (MResult Matrix) sh b
mmap f (Matrix sh v) = Matrix sh $ map f v

mresult :: Matrix () sh a -> Matrix () sh a
mresult = id

mrun :: (Elt a, Elt b, Shape sh) => (Matrix (MNormal Matrix) sh a -> sh -> Exp b) -> Matrix r sh a -> Matrix (MResult Matrix) sh b
mrun f (Matrix sh d) = Matrix sh $ generate (lift $ Z :. toLength sh) $ (\(Z :. ix) -> f (Matrix sh v) $ fromIndex sh ix) . unlift
    where !v = compute d

msize :: Matrix r sh a -> sh
msize (Matrix sh a) = sh

mzipWith :: (Elt a, Elt b, Elt c, Shape sh) => (Exp a -> Exp b -> Exp c) -> Matrix r sh a -> Matrix s sh b -> Matrix (MResult Matrix) sh c
mzipWith f (Matrix sha a) (Matrix shb b) = Matrix sha $ zipWith f a b

