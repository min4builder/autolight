{-# LANGUAGE BangPatterns #-}
module ImgComonad where

import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy
import Control.Comonad (Comonad, extend, extract)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Vector (Vector, (!), generate)
import Data.Vector.Generic (convert)
import Data.Word (Word8)

--- HELPERS

class PixelAdressable i where
    pixel :: i a -> a -> Int -> Int -> a
    inside :: i a -> Int -> Int -> Bool

--- IMAGE HANDLING

data Image a = Image {
    iWidth :: !Int,
    iHeight :: !Int,
    iData :: Vector a }

instance Functor Image where
    fmap f (Image w h d) = Image w h (fmap f d)

instance PixelAdressable Image where
    {-# INLINEABLE pixel #-}
    pixel img@(Image w h d) v !x !y
        | inside img x y = d ! (y * w + x)
        | otherwise = v
    {-# INLINEABLE inside #-}
    inside (Image w h _) !x !y = x >= 0 && y >= 0 && x < w && y < h

readImage :: FilePath -> IO (Image Word8)
readImage filePath = do
    imageR <- Juicy.readImage filePath
    case imageR of
        Right img ->
            return $ Image (Juicy.dynamicMap Juicy.imageWidth img)
                           (Juicy.dynamicMap Juicy.imageHeight img)
                           $ convert $ Juicy.imageData $ Juicy.extractLumaPlane
                               $ Juicy.convertRGB8 img
        Left err -> error $ "readImage: could not load image: " ++ err

writePng :: FilePath -> Image Word8 -> IO ()
writePng filePath img =
    Juicy.writePng filePath
        (Juicy.Image (iWidth img) (iHeight img) (convert $ iData img) :: Juicy.Image Juicy.Pixel8)

writeGifAnim :: FilePath -> [Image Word8] -> IO ()
writeGifAnim filePath imgs = fromRight undefined $
    Juicy.writeGifAnimation filePath 10 Juicy.LoopingNever $ map (\img ->
        Juicy.convertRGB8 $ Juicy.ImageY8 (Juicy.Image (iWidth img) (iHeight img) (convert $ iData img) :: Juicy.Image Juicy.Pixel8)) imgs

--- IMAGE COMONAD

data FocusedImage a = FocusedImage {
    unfocus :: !(Image a),
    focusX :: !Int,
    focusY :: !Int }

focus :: Image a -> FocusedImage a
focus img
    | iWidth img > 0 && iHeight img > 0 = FocusedImage img 0 0
    | otherwise = error "Cannot focus empty images"

dmap :: (a -> b -> c) -> FocusedImage a -> FocusedImage b -> FocusedImage c
dmap f (FocusedImage a _ _) (FocusedImage b@(Image w h _) x y) =
    FocusedImage
        (Image w h $ generate (w * h) $ \index ->
            let (y', x') = index `divMod` w in
            f (pixel a undefined x' y') (pixel b undefined x' y'))
        x y

instance Functor FocusedImage where
    fmap f (FocusedImage img x y) = FocusedImage (fmap f img) x y

instance Comonad FocusedImage where
    extract (FocusedImage img x y) = pixel img undefined x y
    extend f (FocusedImage img@(Image w h _) x y) = FocusedImage
        (Image w h $ generate (w * h) $ \index ->
            let (y', x') = index `divMod` w in
            f (FocusedImage img x' y'))
        x y

instance PixelAdressable FocusedImage where
    {-# INLINEABLE pixel #-}
    pixel (FocusedImage img fx fy) d x y = pixel img d (fx + x) (fy + y)
    {-# INLINEABLE inside #-}
    inside (FocusedImage img fx fy) x y = inside img (fx + x) (fy + y)

