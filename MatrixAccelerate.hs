{-# LANGUAGE FlexibleContexts, FlexibleInstances, RebindableSyntax, TypeFamilies, TypeOperators #-}
module MatrixAccelerate (
    Matrix(..), MResult, MNormal, Shape, SInt,
    Dim1, Dim2,
    zero, toIndex, fromIndex, toLength, sinside, off,
    mindex, minside, mmap, mresult, mrun, msize, mzipWith,
    Elt, Exp, Bool(..), Int, Float,
    fromInteger, fromIntegral, fromRational, negate, ifThenElse, round,
    exp, sqrt, pi, signum,
    lift, unlift, constant,
    (==), (/=), ($), (+), (-), (*), (/), (**), (<), (>), (.), (&&), (||),
) where

import Data.Array.Accelerate hiding (Matrix, Shape, tail, toIndex, fromIndex, minimum, size, sum)
import qualified Data.Array.Accelerate as A
import Prelude (head, tail, id)
import qualified Prelude as P

class Shape sh where
    type SInt sh :: *
    type SInt sh = Exp Int
    zero :: sh
    toIndex :: sh -> sh -> SInt sh
    fromIndex :: sh -> SInt sh -> sh
    toLength :: sh -> SInt sh
    sinside :: sh -> sh -> Exp Bool
    off :: sh -> sh -> sh

type Dim1 = Int
instance Shape (Exp Int) where
    zero = 0
    toIndex _ x = x
    fromIndex _ s = s
    toLength x = x
    sinside s x = 0 <= x && x < s
    off = (+)
instance Shape Int where
    type SInt Int = Int
    zero = 0
    toIndex _ x = x
    fromIndex _ s = s
    toLength x = x
    sinside s x = constant $ 0 P.<= x P.&& x P.< s
    off s x = s + x
type Dim2 = (Int, Int)
instance Shape (Exp (Int, Int)) where
    zero = constant (0, 0)
    toIndex st xy = fst xy + fst st * snd xy
    fromIndex xy s = lift (s `mod` fst xy, s `div` fst xy)
    toLength xy = fst xy * snd xy
    sinside st xy = 0 <= fst xy && fst xy < fst st && 0 <= snd xy && snd xy < snd st
    off st xy = lift (fst st + fst xy, snd st + snd xy)
instance Shape (Int, Int) where
    type SInt (Int, Int) = Int
    zero = (0, 0)
    toIndex (s, t) (x, y) = x + s * y
    fromIndex (x, y) s = (s `mod` x, s `div` x)
    toLength (x, y) = x * y
    sinside (s, t) (x, y) = constant $ 0 P.<= x P.&& x P.< s P.&& 0 P.<= y P.&& y P.< t
    off (s, t) (x, y) = (s + x, t + y)

data Matrix r sh a = Matrix sh (Acc (Array DIM1 a))
type MResult = ()
type MNormal = ()

mindex :: (Elt a, Elt sh, Shape (Exp sh), SInt (Exp sh) ~ Exp Int) => Matrix r sh a -> Exp sh -> Exp a
mindex (Matrix sh v) p = v ! lift (Z :. toIndex (constant sh) p)
{-# INLINEABLE mindex #-}

minside :: (Elt sh, Shape (Exp sh)) => Matrix r sh a -> Exp sh -> Exp Bool
minside v = sinside (constant $ msize v)
{-# INLINEABLE minside #-}

mmap :: (Elt a, Elt b) => (Exp a -> Exp b) -> Matrix r sh a -> Matrix MResult sh b
mmap f (Matrix sh v) = Matrix sh $ map f v

mresult :: Matrix () sh a -> Matrix () sh a
mresult = id

mrun :: (Elt a, Elt b, Elt sh, Shape sh, Shape (Exp sh), SInt sh ~ Int, SInt (Exp sh) ~ Exp Int) => (Matrix MNormal sh a -> Exp sh -> Exp b) -> Matrix r sh a -> Matrix MResult sh b
mrun f (Matrix sh d) = Matrix sh $ generate (lift $ Z :. toLength sh) $ (\(Z :. ix) -> f (Matrix sh v) $ fromIndex (constant sh) ix) . unlift
    where v = compute d

msize :: Matrix r sh a -> sh
msize (Matrix sh a) = sh

mzipWith :: (Elt a, Elt b, Elt c, Shape sh) => (Exp a -> Exp b -> Exp c) -> Matrix r sh a -> Matrix s sh b -> Matrix MResult sh c
mzipWith f (Matrix sha a) (Matrix shb b) = Matrix sha $ zipWith f a b

