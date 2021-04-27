{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Comonad (extend, extract)
import Data.List (foldl')
import System.Environment (getArgs)
import ImgComonad (FocusedImage, dmap, focus, iWidth, iHeight, pixel, readImage, unfocus, writeGifAnim, writePng)

import Criterion.Main

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

autolight img = dmap (*) img $ gaussianBlur 1 shadow
    where (w, h) = (fromIntegral $ iWidth $ unfocus img, fromIntegral $ iHeight $ unfocus img)
          blurr = minimum [w, h] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur blurr img
          delta = fmap (\(dx, dy) -> lx*dx + ly*dy) $ gradient blurry

          dist = distance distr $ fmap (< 0.5) img
          mdist = fmap (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
          shadow = fmap ((+ 0.8) . (* 0.2) . signum) $ dmap (+) delta mdist

gameOfLife :: FocusedImage Bool -> FocusedImage Bool
gameOfLife = extend $ \img ->
    let n = sum [ if pixel img False x y then 1 else 0 | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0) ] in
        n == 3 || (extract img && n == 2)

doautolight (a, b) = do
        img <- readImage "testsmall.png"
        writePng "outsmall.png" $ fromF $ autolight $ toF img

takeiterate 0 _ _ = []
takeiterate !n f a = a : takeiterate (n - 1) f (f a)

dogameOfLife (a, b) = do
        img <- readImage a
        writeGifAnim b $ map fromB $ takeiterate 256 gameOfLife $ toB img

toB = fmap (> 0.5) . toF
fromB = fromF . fmap (\v -> if v then 1 else 0)
toF img = fmap ((/ 256) . fromIntegral) $ focus img :: FocusedImage Float
fromF = unfocus . fmap (toEnum . round . (* 255) . clamp 0 1)

main = defaultMain [
    bgroup "gameOfLife" [
        bench "life0" $ nfIO $ dogameOfLife ("life0.png", "life.gif")
        ],
    bgroup "autolight" [
        bench "testsmall" $ nfIO $ doautolight ("testsmall.png", "outsmall.png")
        ]
    ]

