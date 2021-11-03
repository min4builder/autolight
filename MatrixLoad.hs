{-# LANGUAGE GADTs #-}
module MatrixLoad where

import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import Data.Vector.Generic (convert)
import Data.Word (Word8)
import Matrix

readImage :: FilePath -> IO (Matrix Vector Dim2 Word8)
readImage filePath = do
    imageR <- Juicy.readImage filePath
    case imageR of
        Right img ->
            return $ MatrixVector
                (Juicy.dynamicMap Juicy.imageWidth img
                 :. Juicy.dynamicMap Juicy.imageHeight img)
                    $ convert $ Juicy.imageData $ Juicy.extractLumaPlane
                        $ Juicy.convertRGB8 img
        Left err -> error $ "readImage: could not load image: " ++ err

writePng :: FilePath -> Matrix Vector Dim2 Word8 -> IO ()
writePng filePath (MatrixVector sh v) =
        Juicy.writePng filePath
            (Juicy.Image w h (convert v) :: Juicy.Image Juicy.Pixel8)
    where (w :. h) = sh

writeGifAnim :: FilePath -> [Matrix Vector Dim2 Word8] -> IO ()
writeGifAnim filePath imgs = fromRight undefined $
    Juicy.writeGifAnimation filePath 10 Juicy.LoopingNever $ flip Prelude.map imgs $ \d ->
        let (MatrixVector (w :. h) v) = d in
        Juicy.convertRGB8 $ Juicy.ImageY8
            (Juicy.Image w h $ convert v :: Juicy.Image Juicy.Pixel8)
