{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies, GADTs #-}
module Matrix (
    Dim(..), Dim1, Dim2, Dim3, Dim4, Dim5,
    Matrix(..),
    MNormal,
    MResult,
    Shape,
    fromMassiv,
    fromParallel,
    toMassiv,
    toParallel,
    mindex,
    minside,
    mmap,
    mresult,
    mrun,
    msize,
    mzipWith,
) where

import Control.Monad.Identity (runIdentity)
import qualified Data.Array.Repa as R
import qualified Data.Massiv.Array as M
import Data.Vector as V (Vector, (!), convert, fromList, generate, map, toList, zipWith)
import qualified Data.Vector.Generic as VG (Vector)
import Data.Vector.Unboxed (Unbox)
import Debug.Trace (traceShowId)

data Dim sh = !Int :. !sh deriving (Eq, Show)
type Dim1 = Int
type Dim2 = Dim Dim1
type Dim3 = Dim Dim2
type Dim4 = Dim Dim3
type Dim5 = Dim Dim4

class (Ord sh, Num sh) => Shape sh where
    toIndex :: sh -> sh -> Int
    fromIndex :: sh -> Int -> sh
    size :: sh -> Int

instance Shape Int where
    toIndex a b = b
    fromIndex m n = n
    size = id

instance Ord sh => Ord (Dim sh) where
    compare (a :. r) (b :. s)
        | a < b && r < s = LT
        | a == b && r == s = EQ
        | otherwise = GT

instance Num sh => Num (Dim sh) where
    (+) (a :. r) (b :. s) = (a + b) :. (r + s)
    (*) (a :. r) (b :. s) = (a * b) :. (r * s)
    negate (a :. r) = (-a) :. (-r)
    abs (a :. r) = abs a :. abs r
    signum (a :. r) = signum a :. signum r
    fromInteger 0 = 0 :. 0

instance Shape sh => Shape (Dim sh) where
    toIndex (a :. r) (b :. s) = b + a * toIndex r s
    fromIndex (a :. r) n = (n `mod` a) :. fromIndex r (n `div` a)
    size (a :. r) = a * size r

data Matrix r sh a where
    MatrixVector :: VG.Vector Vector a => sh -> Vector a -> Matrix Vector sh a
--    MatrixArray :: Array r sh a -> Matrix (Array r) sh a
    MatrixParallel :: (Evaluator (R.Array r R.DIM1), R.Source r a) => sh -> R.Array r R.DIM1 a -> Matrix (R.Array r R.DIM1) sh a
    MatrixMassiv :: M.Source r M.Ix1 a => sh -> M.Vector r a -> Matrix (M.Array r M.Ix1) sh a

class Evaluator r where
    revaluate :: Unbox a => Matrix r sh a -> Matrix (MNormal r) sh a

instance Evaluator (R.Array R.U R.DIM1) where
    revaluate = id
instance Evaluator (R.Array R.D R.DIM1) where
    revaluate (MatrixParallel sh v) = MatrixParallel sh $ runIdentity $ R.computeP v

type family MNormal (r :: * -> *) :: * -> * where
    MNormal Vector = Vector
    MNormal (R.Array r R.DIM1) = R.Array R.U R.DIM1
    MNormal (M.Array r M.Ix1) = M.Array M.U M.Ix1

type family MResult (r :: * -> *) :: * -> * where
    MResult Vector = Vector
    MResult (R.Array r R.DIM1) = R.Array R.D R.DIM1
    MResult (M.Array r M.Ix1) = M.Array M.D M.Ix1

toParallel :: (Shape sh, Unbox a) => Matrix Vector sh a -> Matrix (R.Array R.U R.DIM1) sh a
toParallel (MatrixVector sh v) = MatrixParallel sh $ R.fromUnboxed (R.Z R.:. size sh) $ convert v
fromParallel :: Unbox a => Matrix (R.Array r R.DIM1) sh a -> Matrix Vector sh a
fromParallel d@(MatrixParallel _ _) = MatrixVector sh $ convert $ R.toUnboxed v
    where (MatrixParallel sh !v) = revaluate d

toMassiv :: (Shape sh, Unbox a) => Matrix Vector sh a -> Matrix (M.Array M.U M.Ix1) sh a
toMassiv (MatrixVector sh v) = MatrixMassiv sh $ M.fromList M.Par $ toList v
fromMassiv :: Unbox a => Matrix (M.Array r M.Ix1) sh a -> Matrix Vector sh a
fromMassiv (MatrixMassiv sh d) = MatrixVector sh $ fromList $ M.stoList $ M.computeAs M.U d

mindex :: Shape sh => Matrix r sh a -> sh -> a
mindex (MatrixVector sh v) p = v V.! (toIndex sh p)
mindex (MatrixParallel sh v) p = v R.! (R.Z R.:. toIndex sh p)
mindex (MatrixMassiv sh v) p = M.evaluate' v $ toIndex sh p
{-# INLINEABLE mindex #-}

minside :: Shape sh => Matrix r sh a -> sh -> Bool
minside v p = 0 <= p && p < msize v
{-# INLINEABLE minside #-}

mmap :: (a -> b) -> Matrix r sh a -> Matrix (MResult r) sh b
mmap f (MatrixVector sh v) = MatrixVector sh $ V.map f v
mmap f (MatrixParallel sh v) = MatrixParallel sh $ R.map f v
mmap f (MatrixMassiv sh v) = MatrixMassiv sh $ M.map f v

mresult :: Matrix r sh a -> Matrix (MResult r) sh a
mresult (MatrixVector sh v) = MatrixVector sh v
mresult (MatrixParallel sh v) = MatrixParallel sh $ R.delay v
mresult (MatrixMassiv sh v) = MatrixMassiv sh $ M.delay v

mrun :: (Unbox a, Shape sh) => (Matrix (MNormal r) sh a -> sh -> b) -> Matrix r sh a -> Matrix (MResult r) sh b
mrun f d@(MatrixVector sh v) = MatrixVector sh $ generate (size sh) $ f d . fromIndex sh
mrun f d@(MatrixParallel _ _) = MatrixParallel sh $ R.fromFunction (R.Z R.:. size sh) $ \(R.Z R.:. i) -> f vv $ fromIndex sh i
    where vv@(MatrixParallel sh !v) = revaluate d
mrun f (MatrixMassiv sh d) = MatrixMassiv sh $ M.makeArray (M.getComp d) (M.Sz $ size sh) $ \i -> f (MatrixMassiv sh v) $ fromIndex sh i
    where !v = M.compute d

msize :: Matrix r sh a -> sh
msize (MatrixVector sh a) = sh
msize (MatrixParallel sh a) = sh
msize (MatrixMassiv sh a) = sh

mzipWith :: Eq sh => (a -> b -> c) -> Matrix r sh a -> Matrix s sh b -> Matrix (MResult r) sh c
mzipWith f (MatrixVector sha a) (MatrixVector shb b)
    | sha == shb = MatrixVector sha $ V.zipWith f a b
    | otherwise = error "Mismatching shapes"
mzipWith f (MatrixParallel sha a) (MatrixParallel shb b)
    | sha == shb = MatrixParallel sha $ R.zipWith f a b
    | otherwise = error "Mismatching shapes"
mzipWith f (MatrixMassiv sha a) (MatrixMassiv shb b)
    | sha == shb = MatrixMassiv sha $ M.zipWith f a b
    | otherwise = error "Mismatching shapes"

