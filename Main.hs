{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy
import Control.Comonad (Comonad, extract, extend)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Vector (Vector, (!), generate)
import Data.Vector.Generic (convert)
import Data.Word (Word8)
import System.Environment (getArgs)

--- HELPERS

class PixelAdressable i where
    pixel :: i a -> a -> Int -> Int -> a
    inside :: i a -> Int -> Int -> Bool

--- IMAGE HANDLING

data Image a = Image {
    iWidth :: !Int,
    iHeight :: !Int,
    iData :: !(Vector a) }

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
            return Image { 
                iWidth = Juicy.dynamicMap Juicy.imageWidth img,
                iHeight = Juicy.dynamicMap Juicy.imageHeight img,
                iData = convert $ Juicy.imageData $ Juicy.extractLumaPlane $ Juicy.convertRGB8 img }
        Left err -> error $ "readImage: could not load image: " ++ err

writePng :: FilePath -> Image Word8 -> IO ()
writePng filePath img =
    Juicy.writePng filePath (Juicy.Image {
        Juicy.imageWidth = iWidth img,
        Juicy.imageHeight = iHeight img,
        Juicy.imageData = convert $ iData img } :: Juicy.Image Juicy.Pixel8)

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

--- MAIN ALGORITHM

gaussianBlur r = vertical . horizontal
    where add !a !b = a + b
          horizontal = extend $ \img -> foldl' add 0 [ pixel img 0 x 0 * gauss (fromIntegral x) | x <- [-3*(round r) .. 3*(round r)] ]
          vertical = extend $ \img -> foldl' add 0 [ pixel img 0 0 y * gauss (fromIntegral y) | y <- [-3*(round r) .. 3*(round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

gradient = extend $ \img -> (pixel img (extract img) 1 0 - extract img, pixel img (extract img) 0 1 - extract img)

distance r = extend $ \img -> minimum (r : [ sqrt (x*x + y*y) | x <- [-r .. r], y <- [-r .. r],
    pixel img False (round x) (round y) ])

clamp a b c
    | c < a = a
    | c > b = b
    | otherwise = c

main = do
    [a, b] <- getArgs
    img <- readImage a
    let (w, h) = (fromIntegral $ iWidth img, fromIntegral $ iHeight img)
        blurr = minimum [w, h] / 16
        distr = blurr / 2
        distw = 0.01
        (lx, ly) = (1, 1)
        image = fmap ((/ 256) . fromIntegral) $ focus img :: FocusedImage Float

        blurry = gaussianBlur blurr image
        delta = fmap (\(dx, dy) -> lx*dx + ly*dy) $ gradient blurry

        dist = distance distr $ fmap (< 0.5) image
        mdist = fmap (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
        shadow = fmap ((+ 0.8) . (* 0.2) . signum) $ dmap (+) delta mdist

        result = dmap (*) image $ gaussianBlur 1 shadow in
        (writePng b $ unfocus $ fmap (toEnum . round . (* 255) . clamp 0 1) result)

