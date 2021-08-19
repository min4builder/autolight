{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module MatrixComonad where

import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy
import Control.Comonad (Comonad, extend, extract)
import Control.Monad.Identity (runIdentity)
import Data.Array.Repa as R
import Data.Array.Repa.Eval (Load)
import Data.Array.Repa.Repr.Vector
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Kind (Constraint)
import Data.Vector as V (Vector, (!), generate, map, zipWith)
import Data.Vector.Generic (convert)
import Data.Vector.Unboxed (Unbox)
import Data.Word (Word8)

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
    mindex :: Matrix m p sh a => m p sh a -> a -> sh -> a
    minside :: Matrix m p sh a => m p sh a -> sh -> Bool
    minside d p = inShape (msize d) p
    mrun :: (Matrix m p sh a, Matrix m (MResult m) sh b) => (m (MNormal m) sh a -> sh -> b) -> m p sh a -> m (MResult m) sh b
    mnew :: (Matrix m p sh a, p ~ MResult m) => sh -> (sh -> a) -> m p sh a
    mmap :: (Matrix m p sh a, Matrix m (MResult m) sh b) => (a -> b) -> m p sh a -> m (MResult m) sh b
    mzipWith :: (Matrix m p sh a, Matrix m q sh b, Matrix m (MResult m) sh c) => (a -> b -> c) -> m p sh a -> m q sh b -> m (MResult m) sh c
    msize :: Matrix m p sh a => m p sh a -> sh

--- VECTOR PROCESSING MATRIX

data MatrixVector r sh a = MatrixVector { vmShape :: sh, vmData :: Vector a }

instance MatrixImpl MatrixVector () sh a where
    mresult = id
    mindex mat@(MatrixVector sh d) ~v p
        | minside mat p = d V.! (toIndex sh p)
        | otherwise = v
    mrun f d = MatrixVector (vmShape d) $ generate (size $ vmShape d) (f d . fromIndex (vmShape d))
    mnew sh f = MatrixVector sh $ generate (size sh) (f . fromIndex sh)
    mmap f (MatrixVector sh d) = MatrixVector sh $ V.map f d
    mzipWith f (MatrixVector sha a) (MatrixVector shb b)
        | sha == shb = MatrixVector sha $ V.zipWith f a b
        | otherwise = error "Mismatching shapes"
    msize = vmShape

--- SERIAL PROCESSING MATRIX

class RepaEvaluator m r sh a where
    revaluate :: Matrix m r sh a => m r sh a -> m (MNormal m) sh a

newtype MatrixArray r sh a = MatrixArray { smData :: Array r sh a }

toArray :: Unbox a => MatrixVector () sh a -> MatrixArray U sh a
toArray (MatrixVector sh d) = MatrixArray $ fromUnboxed sh $ convert d

fromArray :: (Unbox a, Matrix MatrixArray r sh a, RepaEvaluator MatrixArray r sh a) =>
             MatrixArray r sh a -> MatrixVector () sh a
fromArray d = MatrixVector (msize d) $ convert $ toUnboxed $ smData $ revaluate d

instance RepaEvaluator MatrixArray U sh a where
    revaluate = id

instance Unbox a => RepaEvaluator MatrixArray D sh a where
    revaluate = MatrixArray . computeS . smData

instance RepaEvaluator MatrixArray r sh a => MatrixImpl MatrixArray r sh a where
    type MValid MatrixArray r sh a = Source r a
    type MResult MatrixArray = D
    type MNormal MatrixArray = U
    mresult (MatrixArray d) = MatrixArray $ delay d
    mindex mat@(MatrixArray d) ~v p
        | minside mat p = d R.! p
        | otherwise = v
    mrun f d = let !v = revaluate d in MatrixArray $ fromFunction (msize d) $ f v
    mmap f (MatrixArray d) = MatrixArray $ R.map f d
    mnew sh f = MatrixArray $ fromFunction sh f
    mzipWith f (MatrixArray a) (MatrixArray b) = MatrixArray $ R.zipWith f a b
    msize (MatrixArray d) = extent d

--- PARALLEL PROCESSING MATRIX

newtype Shape sh => MatrixParallel r sh a = MatrixParallel { pmData :: Array r sh a }

instance RepaEvaluator MatrixParallel U sh a where
    revaluate = id

instance Unbox a => RepaEvaluator MatrixParallel D sh a where
    revaluate = MatrixParallel . runIdentity . computeP . pmData

toParallel :: (Unbox a, Shape sh) => MatrixVector () sh a -> MatrixParallel U sh a
toParallel (MatrixVector sh d) = MatrixParallel $ fromUnboxed sh $ convert d

fromParallel :: (Unbox a, Matrix MatrixParallel r sh a, RepaEvaluator MatrixParallel r sh a) =>
                MatrixParallel r sh a -> MatrixVector () sh a
fromParallel d = MatrixVector (msize d) $ convert $ toUnboxed $ pmData $ revaluate d

instance RepaEvaluator MatrixParallel r sh a => MatrixImpl MatrixParallel r sh a where
    type MValid MatrixParallel r sh a = Source r a
    type MResult MatrixParallel = D
    type MNormal MatrixParallel = U
    mresult (MatrixParallel d) = MatrixParallel $ delay d
    mindex mat@(MatrixParallel d) ~v p
        | minside mat p = d R.! p
        | otherwise = v
    mrun f d = let !v = revaluate d in MatrixParallel $ fromFunction (msize d) $ f v
    mnew sh f = MatrixParallel $ fromFunction sh f
    mmap f (MatrixParallel d) = MatrixParallel $ R.map f d
    mzipWith f (MatrixParallel a) (MatrixParallel b) = MatrixParallel $ R.zipWith f a b
    msize (MatrixParallel d) = extent d

--- IMAGE HANDLING

readImage :: FilePath -> IO (MatrixVector () DIM2 Word8)
readImage filePath = do
    imageR <- Juicy.readImage filePath
    case imageR of
        Right img ->
            return $ MatrixVector
                (Z :. Juicy.dynamicMap Juicy.imageWidth img
                   :. Juicy.dynamicMap Juicy.imageHeight img)
                    $ convert $ Juicy.imageData $ Juicy.extractLumaPlane
                        $ Juicy.convertRGB8 img
        Left err -> error $ "readMatrix: could not load image: " Prelude.++ err

writePng :: FilePath -> MatrixVector () DIM2 Word8 -> IO ()
writePng filePath d =
        Juicy.writePng filePath
            (Juicy.Image w h (convert $ vmData d) :: Juicy.Image Juicy.Pixel8)
    where (Z :. w :. h) = msize d

writeGifAnim :: FilePath -> [MatrixVector () DIM2 Word8] -> IO ()
writeGifAnim filePath imgs = fromRight undefined $
    Juicy.writeGifAnimation filePath 10 Juicy.LoopingNever $ Prelude.map (\d ->
        let (Z :. w :. h) = msize d in
        Juicy.convertRGB8 $ Juicy.ImageY8 (Juicy.Image w h (convert $ vmData d) :: Juicy.Image Juicy.Pixel8)) imgs

--- IMAGE COMONAD

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
extract (FocusedMatrix mat p) = mindex mat undefined p

extend :: (Matrix m r sh a, Matrix m (MNormal m) sh a, Matrix m (MResult m) sh b) =>
          (FocusedMatrix m (MNormal m) sh a -> b) -> FocusedMatrix m r sh a -> FocusedMatrix m (MResult m) sh b
extend f (FocusedMatrix mat p) = FocusedMatrix
    (mrun (\mat p -> f $ FocusedMatrix mat p) mat)
    p

index :: Matrix m r sh a => FocusedMatrix m r sh a -> a -> sh -> a
index (FocusedMatrix mat fp) d p = mindex mat d $ addDim fp p

inside :: (Matrix m r sh a, Shape sh) => FocusedMatrix m r sh a -> sh -> Bool
inside (FocusedMatrix mat fp) p = minside mat $ addDim fp p

