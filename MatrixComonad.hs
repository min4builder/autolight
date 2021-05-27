{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module MatrixComonad where

import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy
import Control.Comonad (Comonad, extend, extract)
import Control.Monad.Identity (runIdentity)
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Vector.Generic (convert)
import Data.Word (Word8)

class (Functor m, Shape (MatrixShape m)) => Matrix m where
    type MatrixShape m :: *
    mindex :: m a -> a -> MatrixShape m -> a
    minside :: m a -> MatrixShape m -> Bool
    newMatrix :: MatrixShape m -> (MatrixShape m -> a) -> m a
    mzipWith :: (a -> b -> c) -> m a -> m b -> m c
    msize :: m a -> MatrixShape m

--- SERIAL PROCESSING MATRIX

newtype Shape sh => MatrixArray sh a = MatrixArray { smData :: Array V sh a }

instance Shape sh => Functor (MatrixArray sh) where
    {-# INLINABLE fmap #-}
    fmap f (MatrixArray d) = MatrixArray $ computeS $ Data.Array.Repa.map f d

instance Shape sh => Matrix (MatrixArray sh) where
    type MatrixShape (MatrixArray sh) = sh
    {-# INLINABLE mindex #-}
    mindex mat@(MatrixArray d) v !p
        | minside mat p = d ! p
        | otherwise = v
    {-# INLINABLE minside #-}
    minside (MatrixArray d) !p = inShape (extent d) p
    {-# INLINABLE newMatrix #-}
    newMatrix sh f = MatrixArray $ computeS $ fromFunction sh f
    {-# INLINABLE mzipWith #-}
    mzipWith f (MatrixArray a) (MatrixArray b) = MatrixArray $ computeS $ Data.Array.Repa.zipWith f a b
    {-# INLINABLE msize #-}
    msize (MatrixArray d) = extent d

--- PARALLEL PROCESSING MATRIX

newtype Shape sh => MatrixParallel sh a = MatrixParallel { pmData :: Array V sh a }

toParallel :: Shape sh => MatrixArray sh a -> MatrixParallel sh a
{-# INLINE toParallel #-}
toParallel = MatrixParallel . smData

fromParallel :: Shape sh => MatrixParallel sh a -> MatrixArray sh a
{-# INLINE fromParallel #-}
fromParallel = MatrixArray . pmData

instance Shape sh => Functor (MatrixParallel sh) where
    {-# INLINABLE fmap #-}
    fmap f (MatrixParallel d) = MatrixParallel $ runIdentity $ computeP $ Data.Array.Repa.map f d

instance Shape sh => Matrix (MatrixParallel sh) where
    type MatrixShape (MatrixParallel sh) = sh
    {-# INLINABLE mindex #-}
    mindex mat@(MatrixParallel d) v !p
        | minside mat p = d ! p
        | otherwise = v
    {-# INLINABLE minside #-}
    minside (MatrixParallel d) !p = inShape (extent d) p
    {-# INLINABLE newMatrix #-}
    newMatrix sh f = MatrixParallel $ runIdentity $ computeP $ fromFunction sh f
    {-# INLINABLE mzipWith #-}
    mzipWith f (MatrixParallel a) (MatrixParallel b) = MatrixParallel $ runIdentity $ computeP $ Data.Array.Repa.zipWith f a b
    {-# INLINABLE msize #-}
    msize (MatrixParallel d) = extent d

--- IMAGE HANDLING

readImage :: FilePath -> IO (MatrixArray DIM2 Word8)
readImage filePath = do
    imageR <- Juicy.readImage filePath
    case imageR of
        Right img ->
            return $ MatrixArray $ fromVector
                (Z :. Juicy.dynamicMap Juicy.imageWidth img
                   :. Juicy.dynamicMap Juicy.imageHeight img)
                    $ convert $ Juicy.imageData $ Juicy.extractLumaPlane
                        $ Juicy.convertRGB8 img
        Left err -> error $ "readMatrix: could not load image: " Prelude.++ err

writePng :: FilePath -> MatrixArray DIM2 Word8 -> IO ()
writePng filePath (MatrixArray d) =
        Juicy.writePng filePath
            (Juicy.Image w h (convert $ toVector d) :: Juicy.Image Juicy.Pixel8)
    where (Z :. w :. h) = extent d

writeGifAnim :: FilePath -> [MatrixArray DIM2 Word8] -> IO ()
writeGifAnim filePath imgs = fromRight undefined $
    Juicy.writeGifAnimation filePath 10 Juicy.LoopingNever $ Prelude.map (\(MatrixArray d) ->
        let (Z :. w :. h) = extent d in
        Juicy.convertRGB8 $ Juicy.ImageY8 (Juicy.Image w h (convert $ toVector d) :: Juicy.Image Juicy.Pixel8)) imgs

--- IMAGE COMONAD

data Matrix m => FocusedMatrix m a = FocusedMatrix {
    unfocus :: !(m a),
    focusCoordinates :: !(MatrixShape m) }

focus :: Matrix m => m a -> FocusedMatrix m a
focus mat
    | msize mat /= zeroDim = FocusedMatrix mat zeroDim
    | otherwise = error "Cannot focus empty images"

zipWith :: Matrix m => (a -> b -> c) -> FocusedMatrix m a -> FocusedMatrix m b -> FocusedMatrix m c
{-# INLINABLE zipWith #-}
zipWith f (FocusedMatrix a _) (FocusedMatrix b c) =
    FocusedMatrix (mzipWith f a b) c

instance Matrix m => Functor (FocusedMatrix m) where
    fmap f (FocusedMatrix mat c) = FocusedMatrix (fmap f mat) c

instance Matrix m => Comonad (FocusedMatrix m) where
    extract (FocusedMatrix mat p) = mindex mat undefined p
    {-# INLINABLE extend #-}
    extend f (FocusedMatrix mat p) = FocusedMatrix
        (newMatrix (msize mat) $ \p -> f $ FocusedMatrix mat p)
        p

index :: Matrix m => FocusedMatrix m a -> a -> MatrixShape m -> a
{-# INLINABLE index #-}
index (FocusedMatrix mat fp) d p = mindex mat d $ addDim fp p
{-# SPECIALIZE MatrixComonad.index :: Shape sh => FocusedMatrix (MatrixArray sh) a -> a -> sh -> a #-}
{-# SPECIALIZE MatrixComonad.index :: Shape sh => FocusedMatrix (MatrixParallel sh) a -> a -> sh -> a #-}

inside :: Matrix m => FocusedMatrix m a -> MatrixShape m -> Bool
{-# INLINABLE inside #-}
inside (FocusedMatrix mat fp) p = minside mat $ addDim fp p
{-# SPECIALIZE inside :: Shape sh => FocusedMatrix (MatrixArray sh) a -> sh -> Bool #-}
{-# SPECIALIZE inside :: Shape sh => FocusedMatrix (MatrixParallel sh) a -> sh -> Bool #-}

