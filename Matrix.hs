{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}
module Matrix (
    Matrix,
    MatrixImpl,
    MNormal,
    MValid,
    MResult,
    Shape,
    mindex,
    minside,
    mmap,
    mnew,
    mresult,
    mrun,
    msize,
    mzipWith,
) where

import Data.Kind (Constraint)
import Data.Array.Repa.Shape (Shape, inShape)

type Matrix m p sh a = (Shape sh, MatrixImpl m p sh a, MValid m p sh a,
                        MatrixImpl m (MResult m) sh a, MValid m (MResult m) sh a,
                        MatrixImpl m (MNormal m) sh a, MValid m (MNormal m) sh a)

class MatrixImpl m p sh a where
    type MValid m p sh a :: Constraint
    type MValid m p sh a = MatrixImpl m p sh a
    type MResult m :: *
    type MResult m = ()
    type MNormal m :: *
    type MNormal m = ()
    mresult :: Matrix m p sh a => m p sh a -> m (MResult m) sh a
    mresult = mmap id
    mindex :: Matrix m p sh a => m p sh a -> sh -> a
    minside :: Matrix m p sh a => m p sh a -> sh -> Bool
    minside d p = inShape (msize d) p
    mrun :: (Matrix m p sh a, Matrix m (MResult m) sh b) => (m (MNormal m) sh a -> sh -> b) -> m p sh a -> m (MResult m) sh b
    mnew :: (Matrix m p sh a, p ~ MResult m) => sh -> (sh -> a) -> m p sh a
    mmap :: (Matrix m p sh a, Matrix m (MResult m) sh b) => (a -> b) -> m p sh a -> m (MResult m) sh b
    mzipWith :: (Matrix m p sh a, Matrix m q sh b, Matrix m (MResult m) sh c) => (a -> b -> c) -> m p sh a -> m q sh b -> m (MResult m) sh c
    msize :: Matrix m p sh a => m p sh a -> sh

