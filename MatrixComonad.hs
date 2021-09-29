{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}
module MatrixComonad (
    FocusedMatrix,
    Matrix,
    MatrixArray,
    MatrixMassiv,
    MatrixParallel,
    MatrixVector,
    MNormal,
    MValid,
    MResult,
    extract,
    extend,
    focus,
    fromArray,
    fromMassiv,
    fromParallel,
    index,
    inside,
    MatrixComonad.map,
    mindex,
    minside,
    mmap,
    mnew,
    mresult,
    mrun,
    msize,
    mzipWith,
    readImage,
    toArray,
    toMassiv,
    toParallel,
    vmData,
    mmData,
    unfocus,
    writeGifAnim,
    writePng,
    MatrixComonad.zipWith
) where

import Data.Array.Repa.Shape
import Matrix
import MatrixVector (MatrixVector, readImage, vmData, writePng, writeGifAnim)
import MatrixRepa (MatrixArray, MatrixParallel, fromArray, fromParallel, toArray, toParallel)
import MatrixMassiv (MatrixMassiv, mmData, toMassiv, fromMassiv)

data Matrix m r sh a => FocusedMatrix m r sh a = FocusedMatrix {
    unfocus :: m r sh a,
    focusCoordinates :: sh }

focus :: Matrix m r sh a => m r sh a -> FocusedMatrix m r sh a
focus mat
    | msize mat /= zeroDim = FocusedMatrix mat zeroDim
    | otherwise = error "Cannot focus empty images"

zipWith :: (Matrix m r sh a, Matrix m s sh b, Matrix m (MResult m) sh c) =>
           (a -> b -> c) -> FocusedMatrix m r sh a -> FocusedMatrix m s sh b -> FocusedMatrix m (MResult m) sh c
zipWith f (FocusedMatrix a _) (FocusedMatrix b c) =
    FocusedMatrix (mzipWith f a b) c

map :: (Matrix m r sh a, Matrix m (MResult m) sh b) =>
       (a -> b) -> FocusedMatrix m r sh a -> FocusedMatrix m (MResult m) sh b
map f (FocusedMatrix mat c) = FocusedMatrix (mmap f mat) c

extract :: Matrix m r sh a => FocusedMatrix m r sh a -> a
extract (FocusedMatrix mat p) = mindex mat p

extend :: (Matrix m r sh a, Matrix m (MNormal m) sh a, Matrix m (MResult m) sh b) =>
          (FocusedMatrix m (MNormal m) sh a -> b) -> FocusedMatrix m r sh a -> FocusedMatrix m (MResult m) sh b
extend f (FocusedMatrix mat p) = FocusedMatrix
    (mrun (\mat p -> f $ FocusedMatrix mat p) mat)
    p

index :: Matrix m r sh a => FocusedMatrix m r sh a -> sh -> a
index (FocusedMatrix mat fp) p = mindex mat $ addDim fp p

inside :: Matrix m r sh a => FocusedMatrix m r sh a -> sh -> Bool
inside (FocusedMatrix mat fp) p = minside mat $ addDim fp p

