{-# LANGUAGE BangPatterns #-}
module ImgComonad where

import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy
import Control.Comonad (Comonad, extend, extract)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Vector (Vector, (!), generate, zipWith)
import Data.Vector.Generic (convert)
import Data.Word (Word8)

class Functor i => Image i where
    ipixel :: i a -> a -> Int -> Int -> a
    iinside :: i a -> Int -> Int -> Bool
    newImage :: (Int, Int) -> ((Int, Int) -> a) -> i a
    iZipWith :: (a -> b -> c) -> i a -> i b -> i c
    iSize :: i a -> (Int, Int)

--- IMAGE HANDLING

data ImageArray a = ImageArray {
    iaSize :: !(Int, Int),
    iaData :: Vector a }

instance Functor ImageArray where
    {-# INLINABLE fmap #-}
    fmap f (ImageArray s d) = ImageArray s (fmap f d)

instance Image ImageArray where
    {-# INLINABLE ipixel #-}
    ipixel img@(ImageArray (w, h) d) v !x !y
        | iinside img x y = d ! (y * w + x)
        | otherwise = v
    {-# INLINABLE iinside #-}
    iinside (ImageArray (w, h) _) !x !y = x >= 0 && y >= 0 && x < w && y < h
    {-# INLINABLE newImage #-}
    newImage (w, h) f = ImageArray (w, h) $ generate (w * h) $ \index ->
        let (y, x) = index `divMod` w in f (x, y)
    {-# INLINABLE iZipWith #-}
    iZipWith f a b = ImageArray (iaSize a) $ Data.Vector.zipWith f (iaData a) (iaData b)
    {-# INLINABLE iSize #-}
    iSize = iaSize

readImage :: FilePath -> IO (ImageArray Word8)
readImage filePath = do
    imageR <- Juicy.readImage filePath
    case imageR of
        Right img ->
            return $ ImageArray ((Juicy.dynamicMap Juicy.imageWidth img),
                                 (Juicy.dynamicMap Juicy.imageHeight img))
                                $ convert $ Juicy.imageData $ Juicy.extractLumaPlane
                                    $ Juicy.convertRGB8 img
        Left err -> error $ "readImage: could not load image: " ++ err

writePng :: FilePath -> ImageArray Word8 -> IO ()
writePng filePath (ImageArray (w, h) d) =
    Juicy.writePng filePath
        (Juicy.Image w h (convert d) :: Juicy.Image Juicy.Pixel8)

writeGifAnim :: FilePath -> [ImageArray Word8] -> IO ()
writeGifAnim filePath imgs = fromRight undefined $
    Juicy.writeGifAnimation filePath 10 Juicy.LoopingNever $ map (\(ImageArray (w, h) d) ->
        Juicy.convertRGB8 $ Juicy.ImageY8 (Juicy.Image w h (convert d) :: Juicy.Image Juicy.Pixel8)) imgs

--- IMAGE COMONAD

data Image i => FocusedImage i a = FocusedImage {
    unfocus :: !(i a),
    focusCoordinates :: !(Int, Int) }

focus :: Image i => i a -> FocusedImage i a
focus img
    | iSize img /= (0, 0) = FocusedImage img (0, 0)
    | otherwise = error "Cannot focus empty images"

zipWith :: Image i => (a -> b -> c) -> FocusedImage i a -> FocusedImage i b -> FocusedImage i c
{-# INLINABLE zipWith #-}
zipWith f (FocusedImage a _) (FocusedImage b c) =
    FocusedImage (iZipWith f a b) c
{-# SPECIALIZE ImgComonad.zipWith :: (a -> b -> c) -> FocusedImage ImageArray a -> FocusedImage ImageArray b -> FocusedImage ImageArray c #-}

instance Image i => Functor (FocusedImage i) where
    fmap f (FocusedImage img c) = FocusedImage (fmap f img) c

instance Image i => Comonad (FocusedImage i) where
    extract (FocusedImage img (x, y)) = ipixel img undefined x y
    {-# INLINABLE extend #-}
    extend f (FocusedImage img c) = FocusedImage
        (newImage (iSize img) $ \c -> f $ FocusedImage img c)
        c
    {-# SPECIALIZE extend :: (FocusedImage ImageArray a -> b) -> FocusedImage ImageArray a -> FocusedImage ImageArray b #-}

pixel :: Image i => FocusedImage i a -> a -> Int -> Int -> a
{-# INLINABLE pixel #-}
pixel (FocusedImage img (fx, fy)) d x y = ipixel img d (fx + x) (fy + y)
{-# SPECIALIZE pixel :: FocusedImage ImageArray a -> a -> Int -> Int -> a #-}

inside :: Image i => FocusedImage i a -> Int -> Int -> Bool
{-# INLINABLE inside #-}
inside (FocusedImage img (fx, fy)) x y = iinside img (fx + x) (fy + y)
{-# SPECIALIZE inside :: FocusedImage ImageArray a -> Int -> Int -> Bool #-}

