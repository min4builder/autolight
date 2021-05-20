{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Comonad (extend, extract)
import Data.List (foldl')
import Data.Word (Word8)
import ImgComonad

import Criterion.Main

gaussianBlur r = vertical . horizontal
    where add !a !b = a + b
          horizontal = extend $ \img -> foldl' add 0 [ gauss (fromIntegral x) * pixel img 0 x 0 | x <- [-3*(round r) .. 3*(round r)] ]
          vertical = extend $ \img -> foldl' add 0 [ gauss (fromIntegral y) * pixel img 0 0 y | y <- [-3*(round r) .. 3*(round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

gaussianBlur' r = vertical . horizontal
    where add !a !b = a + b
          horizontal img = newImage (iSize img) $ \(x, y) ->
              foldl' add 0 [ gauss (fromIntegral (x' - x)) * ipixel img 0 x' y | x' <- [x - 3*(round r) .. x + 3*(round r)] ]
          vertical img = newImage (iSize img) $ \(x, y) ->
              foldl' add 0 [ gauss (fromIntegral (y' - y)) * ipixel img 0 x y' | y' <- [y - 3*(round r) .. y + 3*(round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

gradient = extend $ \img ->
    let v = extract img in (pixel img v 1 0 - v, pixel img v 0 1 - v)

gradient' img = newImage (iSize img) $ \(x, y) ->
    let v = ipixel img undefined x y in (ipixel img v (x + 1) y - v, ipixel img v x (y + 1) - v)

distance r = extend $ \img ->
    minimum (r : [ sqrt (x*x + y*y) | x <- [-r .. r], y <- [-r .. r],
        pixel img False (round x) (round y) ])

distance' r img = newImage (iSize img) $ \(x, y) ->
    minimum (r : [ sqrt (x'*x' + y'*y') | x' <- [(fromIntegral x) - r .. (fromIntegral x) + r], y' <- [(fromIntegral y) - r .. (fromIntegral y) + r],
        ipixel img False (round x') (round y') ])

autolight img = ImgComonad.zipWith (*) img $ gaussianBlur 1 shadow
    where (w, h) = iSize $ unfocus img
          blurr = minimum [fromIntegral w, fromIntegral h] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur blurr img
          delta = fmap (\(dx, dy) -> lx*dx + ly*dy) $ gradient blurry

          dist = distance distr $ fmap (< 0.5) img
          mdist = fmap (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
          shadow = fmap ((+ 0.8) . (* 0.2) . signum) $ ImgComonad.zipWith (+) delta mdist

autolight' img = iZipWith (*) img $ gaussianBlur' 1 shadow
    where (w, h) = iSize img
          blurr = minimum [fromIntegral w, fromIntegral h] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur' blurr img
          delta = fmap (\(dx, dy) -> lx*dx + ly*dy) $ gradient' blurry

          dist = distance' distr $ fmap (< 0.5) img
          mdist = fmap (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
          shadow = fmap ((+ 0.8) . (* 0.2) . signum) $ iZipWith (+) delta mdist

gameOfLife = extend $ \img ->
    let n = sum [ if pixel img False x y then 1 else 0 | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0) ] in
        n == 3 || (extract img && n == 2)

gameOfLife' img = newImage (iSize img) $ \(x, y) ->
    let n = sum [ if ipixel img False (x + dx) (y + dy) then 1 else 0 | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0) ] in
        n == 3 || (ipixel img False x y && n == 2)

takeiterate 0 _ _ = []
takeiterate !n f a = a : takeiterate (n - 1) f (f a)

toB = fmap (> 0.5) . toF
fromB = fromF . fmap (\v -> if v then 1 else 0)
toF = fmap ((/ 256) . fromIntegral)
fromF = fmap (toEnum . round . (* 255) . clamp 0 1)
clamp a b c
    | c < a = a
    | c > b = b
    | otherwise = c

main = do
    testsmall <- readImage "testsmall.png"
    testbig <- readImage "testbig.png"
    life0 <- readImage "life0.png"
    defaultMain [
        bgroup "gaussianBlur" [
            bench "testsmall" $ nf (iData . unfocus . gaussianBlur 16) $ focus $ toF testsmall,
            bench "testbig" $ nf (iData . unfocus . gaussianBlur 16) $ focus $ toF testbig
            ],
        bgroup "gaussianBlur'" [
            bench "testsmall" $ nf (iData . gaussianBlur' 16) $ toF testsmall,
            bench "testbig" $ nf (iData . gaussianBlur' 16) $ toF testbig
            ],
        bgroup "gameOfLife" [
            bench "life0" $ nf (map (iData . unfocus) . takeiterate 16 gameOfLife) $ focus $ toB life0
            ],
        bgroup "gameOfLife'" [
            bench "life0" $ nf (map iData . takeiterate 16 gameOfLife') $ toB life0
            ]
        ]

