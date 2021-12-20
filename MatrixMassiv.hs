{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module MatrixMassiv (
    Matrix(..), MResult, MNormal, Shape,
    Dim1, Dim2, Dim3, Dim4, Dim5,
    zero, toIndex, fromIndex, toLength, sinside, off,
    mindex, minside, mmap, mresult, mrun, msize, mzipWith,
    Ix1, Load, Manifest, Source, Unbox,
) where

import Control.Monad.Identity (runIdentity)
import Data.Massiv.Array as M hiding (Matrix, Shape, fromIndex, msize, toIndex)
import Data.Vector.Unboxed (Unbox)
import Shape

data Matrix r sh a = Matrix sh (Array r Ix1 a)
type MNormal = U
type MResult = D

mindex :: (Shape sh, Unbox a) => Matrix U sh a -> sh -> a
mindex (Matrix sh v) p = index' v $ toIndex sh p
{-# INLINEABLE mindex #-}

minside :: Shape sh => Matrix r sh a -> sh -> Bool
minside v p = sinside (msize v) p
{-# INLINEABLE minside #-}

mmap :: (Source r Ix1 a) => (a -> b) -> Matrix r sh a -> Matrix MResult sh b
mmap f (Matrix sh v) = Matrix sh $ M.map f v

mresult :: (Source r Ix1 a) => Matrix r sh a -> Matrix MResult sh a
mresult (Matrix sh v) = Matrix sh $ delay v

mrun :: (Load r Ix1 a, Unbox a, Shape sh) => (Matrix MNormal sh a -> sh -> b) -> Matrix r sh a -> Matrix MResult sh b
mrun f (Matrix sh d) = Matrix sh $ makeArray (getComp d) (Sz $ toLength sh) $ f (Matrix sh v) . fromIndex sh
    where v = compute d

msize :: Matrix r sh a -> sh
msize (Matrix sh a) = sh

mzipWith :: (Eq sh, Source r Ix1 a, Source s Ix1 b) => (a -> b -> c) -> Matrix r sh a -> Matrix s sh b -> Matrix MResult sh c
mzipWith f (Matrix sha a) (Matrix shb b)
    | sha == shb = Matrix sha $ M.zipWith f a b
    | otherwise = error "Mismatching shapes"

