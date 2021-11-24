{-# LANGUAGE TypeFamilies #-}
module MatrixVector (
    Matrix(..), MResult, MNormal, Shape,
    Dim1, Dim2, Dim3, Dim4, Dim5,
    zero, toIndex, fromIndex, toLength, sinside, off,
    mindex, minside, mmap, mresult, mrun, msize, mzipWith,
    Unbox,
) where

import Data.Vector as V (Vector, (!), convert, fromList, generate, map, toList, zipWith)
import Data.Vector.Unboxed (Unbox)
import Shape

data Matrix r sh a = Matrix sh (Vector a)
type instance MNormal Matrix = ()
type instance MResult Matrix = ()

mindex :: Shape sh => Matrix r sh a -> sh -> a
mindex (Matrix sh v) p = v V.! (toIndex sh p)
{-# INLINEABLE mindex #-}

minside :: Shape sh => Matrix r sh a -> sh -> Bool
minside v p = sinside (msize v) p
{-# INLINEABLE minside #-}

mmap :: (a -> b) -> Matrix r sh a -> Matrix (MResult Matrix) sh b
mmap f (Matrix sh v) = Matrix sh $ V.map f v

mresult :: Matrix r sh a -> Matrix (MResult Matrix) sh a
mresult (Matrix sh v) = Matrix sh v

mrun :: (Unbox a, Shape sh) => (Matrix (MNormal Matrix) sh a -> sh -> b) -> Matrix r sh a -> Matrix (MResult Matrix) sh b
mrun f (Matrix sh v) = Matrix sh $ generate (toLength sh) $ f (Matrix sh v) . fromIndex sh

msize :: Matrix r sh a -> sh
msize (Matrix sh a) = sh

mzipWith :: Eq sh => (a -> b -> c) -> Matrix r sh a -> Matrix s sh b -> Matrix (MResult Matrix) sh c
mzipWith f (Matrix sha a) (Matrix shb b)
    | sha == shb = Matrix sha $ V.zipWith f a b
    | otherwise = error "Mismatching shapes"

