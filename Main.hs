module Main where

import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy
import Data.Maybe (fromJust)
import Data.Vector (Vector, (!), generate)
import Data.Vector.Generic (convert)
import Data.Word (Word8)
import System.Environment (getArgs)

--- HELPERS

infixl 1 >=>

class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
    duplicate = extend id
    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate
    (>=>) :: (w a -> b) -> (w b -> c) -> (w a -> c)
    (>=>) a b = b . extend a

class PixelAdressable i where
    pixel :: i a -> Int -> Int -> Maybe a

valueOr :: Maybe a -> a -> a
valueOr (Just a) _ = a
valueOr Nothing a = a

--- IMAGE HANDLING

data Image a = Image {
    iWidth :: Int,
    iHeight :: Int,
    iData :: Vector a }

instance Functor Image where
    fmap f (Image w h d) = Image w h (fmap f d)

instance PixelAdressable Image where
    pixel (Image w h d) x y
        | x < 0 || y < 0 || x >= w || y >= h = Nothing
        | otherwise = Just $ d ! (y * w + x)

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
    unfocus :: Image a,
    focusX :: Int,
    focusY :: Int }

focus :: Image a -> FocusedImage a
focus img
    | iWidth img > 0 && iHeight img > 0 = FocusedImage img 0 0
    | otherwise = error "Cannot focus empty images"

dmap :: (a -> b -> c) -> FocusedImage a -> FocusedImage b -> FocusedImage c
dmap f (FocusedImage a _ _) (FocusedImage b@(Image w h _) x y) =
    FocusedImage
        (Image w h $ generate (w * h) $ \index ->
            let (y', x') = index `divMod` w in
            f (fromJust $ pixel a x' y') (fromJust $ pixel b x' y'))
        x y

instance Functor FocusedImage where
    fmap f (FocusedImage img x y) = FocusedImage (fmap f img) x y

instance Comonad FocusedImage where
    extract (FocusedImage img x y) = pixel img x y
        `valueOr` error "Cannot focus outside the image"
    extend f (FocusedImage img@(Image w h _) x y) = FocusedImage
        (Image w h $ generate (w * h) $ \index ->
            let (y', x') = index `divMod` w in
            f (FocusedImage img x' y'))
        x y

instance PixelAdressable FocusedImage where
    pixel (FocusedImage img fx fy) x y = pixel img (fx + x) (fy + y)

--- MAIN ALGORITHM

gauss2d x y r = exp (-((x**2 + y**2) / (2 * r**2))) / (2 * pi * r**2)

gaussianBlur r img = sum [ gauss2d x y r * (pixel img (round x) (round y) `valueOr` 0)
    | x <- [-3*r .. 3*r], y <- [-3*r .. 3*r] ]

gradient img = (pixel img 1 0 `valueOr` extract img - extract img, pixel img 0 1 `valueOr` extract img - extract img)

distance r img = minimum (r : [ sqrt (x*x + y*y) | x <- [-r .. r], y <- [-r .. r],
    pixel img (round x) (round y) `valueOr` False ])

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

        blurry = extend (gaussianBlur blurr) image
        delta = fmap (\(dx, dy) -> lx*dx + ly*dy) $ extend gradient blurry

        dist = extend (distance distr) $ fmap (< 0.5) image
        mdist = fmap (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
        shadow = fmap ((+ 0.8) . (* 0.2) . signum) $ dmap (+) delta mdist

        result = dmap (*) image $ extend (gaussianBlur 1) shadow in
        (writePng b $ unfocus $ fmap (toEnum . round . (* 255) . clamp 0 1) result)

