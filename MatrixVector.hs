{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Strict, TypeFamilies #-}
module MatrixVector where

import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Vector as V (Vector, (!), generate, map, zipWith)
import Data.Vector.Generic (convert)
import Data.Word (Word8)
import Matrix

data MatrixVector r sh a = MatrixVector { vmShape :: sh, vmData :: Vector a }

instance Shape sh => MatrixImpl MatrixVector () sh a where
    mresult = id
    minside (MatrixVector sh d) p = inShape sh p
    mindex (MatrixVector sh d) p = d V.! (toIndex sh p)
    mrun f d = MatrixVector (vmShape d) $ generate (size $ vmShape d) (f d . fromIndex (vmShape d))
    mnew sh f = MatrixVector sh $ generate (size sh) (f . fromIndex sh)
    mmap f (MatrixVector sh d) = MatrixVector sh $ V.map f d
    mzipWith f (MatrixVector sha a) (MatrixVector shb b)
        | sha == shb = MatrixVector sha $ V.zipWith f a b
        | otherwise = error "Mismatching shapes"
    msize = vmShape

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
        Left err -> error $ "readImage: could not load image: " ++ err

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

