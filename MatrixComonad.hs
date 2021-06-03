{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module MatrixComonad where

import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy
import Control.Comonad (Comonad, extend, extract)
import Control.Monad.Identity (runIdentity)
import Data.Array.Repa
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Vector (Vector, (!), generate, zipWith)
import qualified Data.Vector as V
import Data.Vector.Generic (convert)
import Data.Word (Word8)

class (Functor m, Shape (MatrixShape m)) => Matrix m where
    type MatrixShape m :: *
    mindex :: m a -> a -> MatrixShape m -> a
    minside :: m a -> MatrixShape m -> Bool
    newMatrix :: MatrixShape m -> (MatrixShape m -> a) -> m a
    mzipWith :: (a -> b -> c) -> m a -> m b -> m c
    msize :: m a -> MatrixShape m

--- VECTOR PROCESSING MATRIX

data Shape sh => MatrixVector sh a = MatrixVector { vmShape :: !sh, vmData :: !(Vector a) }


instance Shape sh => Functor (MatrixVector sh) where
    fmap f (MatrixVector sh d) = MatrixVector sh $ fmap f d

instance Shape sh => Matrix (MatrixVector sh) where
    type MatrixShape (MatrixVector sh) = sh
    mindex mat@(MatrixVector sh d) v !p
        | minside mat p = d V.! (toIndex sh p)
        | otherwise = v
    minside d !p = inShape (vmShape d) p
    newMatrix sh f = MatrixVector sh $ generate (size sh) (f . fromIndex sh)
    mzipWith f (MatrixVector sha a) (MatrixVector shb b)
        | sha == shb = MatrixVector sha $ V.zipWith f a b
        | otherwise = error "Mismatching shapes"
    msize = vmShape

--- SERIAL PROCESSING MATRIX

newtype Shape sh => MatrixArray sh a = MatrixArray { smData :: Array V sh a }

toArray :: Shape sh => MatrixVector sh a -> MatrixArray sh a
toArray (MatrixVector sh d) = MatrixArray $ fromVector sh d

fromArray :: Shape sh => MatrixArray sh a -> MatrixVector sh a
fromArray (MatrixArray d) = MatrixVector (extent d) $ toVector d

instance Shape sh => Functor (MatrixArray sh) where
    fmap f (MatrixArray d) = MatrixArray $ computeS $ R.map f d

instance Shape sh => Matrix (MatrixArray sh) where
    type MatrixShape (MatrixArray sh) = sh
    mindex mat@(MatrixArray d) v !p
        | minside mat p = d R.! p
        | otherwise = v
    minside (MatrixArray d) !p = inShape (extent d) p
    newMatrix sh f = MatrixArray $ computeS $ fromFunction sh f
    mzipWith f (MatrixArray a) (MatrixArray b) = MatrixArray $ computeS $ R.zipWith f a b
    msize (MatrixArray d) = extent d

--- PARALLEL PROCESSING MATRIX

newtype Shape sh => MatrixParallel sh a = MatrixParallel { pmData :: Array V sh a }

toParallel :: Shape sh => MatrixVector sh a -> MatrixParallel sh a
toParallel (MatrixVector sh d) = MatrixParallel $ fromVector sh d

fromParallel :: Shape sh => MatrixParallel sh a -> MatrixVector sh a
fromParallel (MatrixParallel d) = MatrixVector (extent d) $ toVector d

instance Shape sh => Functor (MatrixParallel sh) where
    fmap f (MatrixParallel d) = MatrixParallel $ runIdentity $ computeP $ R.map f d

instance Shape sh => Matrix (MatrixParallel sh) where
    type MatrixShape (MatrixParallel sh) = sh
    mindex mat@(MatrixParallel d) v !p
        | minside mat p = d R.! p
        | otherwise = v
    minside (MatrixParallel d) !p = inShape (extent d) p
    newMatrix sh f = MatrixParallel $ runIdentity $ computeP $ fromFunction sh f
    mzipWith f (MatrixParallel a) (MatrixParallel b) = MatrixParallel $ runIdentity $ computeP $ R.zipWith f a b
    msize (MatrixParallel d) = extent d

--- IMAGE HANDLING

readImage :: FilePath -> IO (MatrixVector DIM2 Word8)
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

writePng :: FilePath -> MatrixVector DIM2 Word8 -> IO ()
writePng filePath d =
        Juicy.writePng filePath
            (Juicy.Image w h (convert $ vmData d) :: Juicy.Image Juicy.Pixel8)
    where (Z :. w :. h) = msize d

writeGifAnim :: FilePath -> [MatrixVector DIM2 Word8] -> IO ()
writeGifAnim filePath imgs = fromRight undefined $
    Juicy.writeGifAnimation filePath 10 Juicy.LoopingNever $ Prelude.map (\d ->
        let (Z :. w :. h) = msize d in
        Juicy.convertRGB8 $ Juicy.ImageY8 (Juicy.Image w h (convert $ vmData d) :: Juicy.Image Juicy.Pixel8)) imgs

--- IMAGE COMONAD

data Matrix m => FocusedMatrix m a = FocusedMatrix {
    unfocus :: !(m a),
    focusCoordinates :: !(MatrixShape m) }

focus :: Matrix m => m a -> FocusedMatrix m a
focus mat
    | msize mat /= zeroDim = FocusedMatrix mat zeroDim
    | otherwise = error "Cannot focus empty images"

zipWith :: Matrix m => (a -> b -> c) -> FocusedMatrix m a -> FocusedMatrix m b -> FocusedMatrix m c
zipWith f (FocusedMatrix a _) (FocusedMatrix b c) =
    FocusedMatrix (mzipWith f a b) c

instance Matrix m => Functor (FocusedMatrix m) where
    fmap f (FocusedMatrix mat c) = FocusedMatrix (fmap f mat) c

instance Matrix m => Comonad (FocusedMatrix m) where
    extract (FocusedMatrix mat p) = mindex mat undefined p
    extend f (FocusedMatrix mat p) = FocusedMatrix
        (newMatrix (msize mat) $ \p -> f $ FocusedMatrix mat p)
        p

index :: Matrix m => FocusedMatrix m a -> a -> MatrixShape m -> a
index (FocusedMatrix mat fp) d p = mindex mat d $ addDim fp p
{-# SPECIALIZE MatrixComonad.index :: Shape sh => FocusedMatrix (MatrixVector sh) a -> a -> sh -> a #-}
{-# SPECIALIZE MatrixComonad.index :: Shape sh => FocusedMatrix (MatrixArray sh) a -> a -> sh -> a #-}
{-# SPECIALIZE MatrixComonad.index :: Shape sh => FocusedMatrix (MatrixParallel sh) a -> a -> sh -> a #-}

inside :: Matrix m => FocusedMatrix m a -> MatrixShape m -> Bool
inside (FocusedMatrix mat fp) p = minside mat $ addDim fp p
{-# SPECIALIZE inside :: Shape sh => FocusedMatrix (MatrixVector sh) a -> sh -> Bool #-}
{-# SPECIALIZE inside :: Shape sh => FocusedMatrix (MatrixArray sh) a -> sh -> Bool #-}
{-# SPECIALIZE inside :: Shape sh => FocusedMatrix (MatrixParallel sh) a -> sh -> Bool #-}

