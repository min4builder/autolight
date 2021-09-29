{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module MatrixRepa where

import Control.Comonad (Comonad, extend, extract)
import Control.Monad.Identity (runIdentity)
import Data.Array.Repa as R
import Data.Array.Repa.Eval (Load)
import Data.Vector.Generic (convert)
import Data.Vector.Unboxed (Unbox)
import Matrix
import MatrixVector

class RepaEvaluator m r a where
    revaluate :: (Shape sh, Matrix m r sh a) => m r sh a -> m (MNormal m) sh a

newtype MatrixArray r sh a = MatrixArray { smData :: Array r sh a }

toArray :: Unbox a => MatrixVector () sh a -> MatrixArray U sh a
toArray (MatrixVector sh d) = MatrixArray $ fromUnboxed sh $ convert d

fromArray :: (Unbox a, Shape sh, Matrix MatrixArray r sh a, RepaEvaluator MatrixArray r a) =>
             MatrixArray r sh a -> MatrixVector () sh a
fromArray d = MatrixVector (msize d) $ convert $ toUnboxed $ smData $ revaluate d

instance RepaEvaluator MatrixArray U a where
    revaluate = id

instance Unbox a => RepaEvaluator MatrixArray D a where
    revaluate = MatrixArray . computeS . smData

instance (Shape sh, RepaEvaluator MatrixArray r a) => MatrixImpl MatrixArray r sh a where
    type MValid MatrixArray r sh a = Source r a
    type MResult MatrixArray = D
    type MNormal MatrixArray = U
    mresult (MatrixArray d) = MatrixArray $ delay d
    minside d p = inShape (msize d) p
    mindex (MatrixArray d) p = d ! p
    mrun f d = let !v = revaluate d in MatrixArray $ fromFunction (msize d) $ f v
    mmap f (MatrixArray d) = MatrixArray $ R.map f d
    mnew sh f = MatrixArray $ fromFunction sh f
    mzipWith f (MatrixArray a) (MatrixArray b) = MatrixArray $ R.zipWith f a b
    msize (MatrixArray d) = extent d

newtype Shape sh => MatrixParallel r sh a = MatrixParallel { pmData :: Array r sh a }

instance RepaEvaluator MatrixParallel U a where
    revaluate = id

instance Unbox a => RepaEvaluator MatrixParallel D a where
    revaluate = MatrixParallel . runIdentity . computeP . pmData

toParallel :: (Unbox a, Shape sh) => MatrixVector () sh a -> MatrixParallel U sh a
toParallel (MatrixVector sh d) = MatrixParallel $ fromUnboxed sh $ convert d

fromParallel :: (Unbox a, Shape sh, Matrix MatrixParallel r sh a, RepaEvaluator MatrixParallel r a) =>
                MatrixParallel r sh a -> MatrixVector () sh a
fromParallel d = MatrixVector (msize d) $ convert $ toUnboxed $ pmData $ revaluate d

instance (Shape sh, RepaEvaluator MatrixParallel r a) => MatrixImpl MatrixParallel r sh a where
    type MValid MatrixParallel r sh a = Source r a
    type MResult MatrixParallel = D
    type MNormal MatrixParallel = U
    mresult (MatrixParallel d) = MatrixParallel $ delay d
    minside d p = inShape (msize d) p
    mindex (MatrixParallel d) p = d ! p
    mrun f d = let !v = revaluate d in MatrixParallel $ fromFunction (msize d) $ f v
    mnew sh f = MatrixParallel $ fromFunction sh f
    mmap f (MatrixParallel d) = MatrixParallel $ R.map f d
    mzipWith f (MatrixParallel a) (MatrixParallel b) = MatrixParallel $ R.zipWith f a b
    msize (MatrixParallel d) = extent d

