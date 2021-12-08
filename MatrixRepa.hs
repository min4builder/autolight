{-# LANGUAGE BangPatterns #-}
module MatrixRepa (
    Matrix(..), MResult, MNormal, Shape,
    Dim1, Dim2, Dim3, Dim4, Dim5,
    zero, toIndex, fromIndex, toLength, sinside, off,
    mindex, minside, mmap, mresult, mrun, msize, mzipWith,
    Evaluator, Source, Unbox, evaluate,
) where

import Control.Monad.Identity (runIdentity)
import Data.Array.Repa as R hiding (Shape, fromIndex, toIndex)
import Data.Vector.Unboxed (Unbox)
import Shape

class Evaluator r where
    evaluate :: Unbox a => Matrix r sh a -> Matrix MNormal sh a

instance Evaluator U where
    evaluate = id
instance Evaluator D where
    evaluate (Matrix sh v) = Matrix sh $ runIdentity $ computeP v

data Matrix r sh a = Matrix sh (Array r DIM1 a)
type MNormal = U
type MResult = D

mindex :: (Source r a, Shape sh) => Matrix r sh a -> sh -> a
mindex (Matrix sh v) p = v ! (Z :. toIndex sh p)
{-# INLINEABLE mindex #-}

minside :: Shape sh => Matrix r sh a -> sh -> Bool
minside v p = sinside (msize v) p
{-# INLINEABLE minside #-}

mmap :: Source r a => (a -> b) -> Matrix r sh a -> Matrix MResult sh b
mmap f (Matrix sh v) = Matrix sh $ R.map f v

mresult :: Source r a => Matrix r sh a -> Matrix MResult sh a
mresult (Matrix sh v) = Matrix sh $ delay v

mrun :: (Evaluator r, Unbox a, Shape sh) => (Matrix MNormal sh a -> sh -> b) -> Matrix r sh a -> Matrix MResult sh b
mrun f d = Matrix sh $ fromFunction (Z :. toLength sh) $ \(Z :. i) -> f v $ fromIndex sh i
    where v@(Matrix sh !_) = evaluate d

msize :: Matrix r sh a -> sh
msize (Matrix sh a) = sh

mzipWith :: (Eq sh, Source r a, Source s b) => (a -> b -> c) -> Matrix r sh a -> Matrix s sh b -> Matrix MResult sh c
mzipWith f (Matrix sha a) (Matrix shb b)
    | sha == shb = Matrix sha $ R.zipWith f a b
    | otherwise = error "Mismatching shapes"

