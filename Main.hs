{-# LANGUAGE BangPatterns, TypeFamilies #-}
module Main where

import Control.Comonad (extend, extract)
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Array.Repa.Repr.Vector (toVector)
import Data.List (foldl')
import Data.Word (Word8)
import MatrixComonad

import Criterion.Main

gaussianBlur r = vertical . horizontal
    where add !a !b = a + b
          horizontal = extend $ \img -> foldl' add 0 [ gauss (fromIntegral x) * index img 0 (ix2 x 0) | x <- [-3*(round r) .. 3*(round r)] ]
          vertical = extend $ \img -> foldl' add 0 [ gauss (fromIntegral y) * index img 0 (ix2 0 y) | y <- [-3*(round r) .. 3*(round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

gaussianBlur' r = vertical . horizontal
    where add !a !b = a + b
          horizontal = mrun $ \img (Z :. x :. y) ->
              foldl' add 0 [ gauss (fromIntegral (x' - x)) * mindex img 0 (ix2 x' y) | x' <- [x - 3*(round r) .. x + 3*(round r)] ]
          vertical = mrun $ \img (Z :. x :. y) ->
              foldl' add 0 [ gauss (fromIntegral (y' - y)) * mindex img 0 (ix2 x y') | y' <- [y - 3*(round r) .. y + 3*(round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

gradient :: (Matrix m, MatrixShape m ~ DIM2, Num p) => FocusedMatrix m p -> FocusedMatrix m (p, p)
gradient = extend $ \img ->
    let v = extract img in (index img v (ix2 1 0) - v, index img v (ix2 0 1) - v)

gradient' :: (Matrix m, MatrixShape m ~ DIM2, Num p) => m p -> m (p, p)
gradient' = mrun $ \img (Z :. x :. y) ->
    let v = mindex img undefined (ix2 x y) in (mindex img v (ix2 (x + 1) y) - v, mindex img v (ix2 x (y + 1)) - v)

distance r = extend $ \img ->
    minimum (r : [ sqrt (x*x + y*y) | x <- [-r .. r], y <- [-r .. r],
        index img False (ix2 (round x) (round y)) ])

distance' r = mrun $ \img (Z :. x :. y) ->
    minimum (r : [ sqrt (x'*x' + y'*y') | x' <- [(fromIntegral x) - r .. (fromIntegral x) + r], y' <- [(fromIntegral y) - r .. (fromIntegral y) + r],
        mindex img False (ix2 (round x') (round y')) ])

autolight img = MatrixComonad.zipWith (*) img $ gaussianBlur 1 shadow
    where (Z :. w :. h) = msize $ unfocus img
          blurr = minimum [fromIntegral w, fromIntegral h] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur blurr img
          delta = fmap (\(dx, dy) -> lx*dx + ly*dy) $ gradient blurry

          dist = distance distr $ fmap (< 0.5) img
          mdist = fmap (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
          shadow = fmap ((+ 0.8) . (* 0.2) . signum) $ MatrixComonad.zipWith (+) delta mdist
{-# SPECIALIZE autolight :: FocusedMatrix (MatrixVector DIM2) Float -> FocusedMatrix (MatrixVector DIM2) Float #-}
{-# SPECIALIZE autolight :: FocusedMatrix (MatrixArray DIM2) Float -> FocusedMatrix (MatrixArray DIM2) Float #-}
{-# SPECIALIZE autolight :: FocusedMatrix (MatrixParallel DIM2) Float -> FocusedMatrix (MatrixParallel DIM2) Float #-}

autolight' img = mzipWith (*) img $ gaussianBlur' 1 shadow
    where (Z :. w :. h) = msize img
          blurr = minimum [fromIntegral w, fromIntegral h] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur' blurr img
          delta = fmap (\(dx, dy) -> lx*dx + ly*dy) $ gradient' blurry

          dist = distance' distr $ fmap (< 0.5) img
          mdist = fmap (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
          shadow = fmap ((+ 0.8) . (* 0.2) . signum) $ mzipWith (+) delta mdist
{-# SPECIALIZE autolight' :: MatrixVector DIM2 Float -> MatrixVector DIM2 Float #-}
{-# SPECIALIZE autolight' :: MatrixArray DIM2 Float -> MatrixArray DIM2 Float #-}
{-# SPECIALIZE autolight' :: MatrixParallel DIM2 Float -> MatrixParallel DIM2 Float #-}

gameOfLife :: (Matrix i, MatrixShape i ~ DIM2) => FocusedMatrix i Bool -> FocusedMatrix i Bool
gameOfLife = extend $ \img ->
    let n = sum [ if index img False (ix2 x y) then 1 else 0 | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0) ] in
        n == 3 || (extract img && n == 2)
{-# SPECIALIZE gameOfLife :: FocusedMatrix (MatrixVector DIM2) Bool -> FocusedMatrix (MatrixVector DIM2) Bool #-}
{-# SPECIALIZE gameOfLife :: FocusedMatrix (MatrixArray DIM2) Bool -> FocusedMatrix (MatrixArray DIM2) Bool #-}
{-# SPECIALIZE gameOfLife :: FocusedMatrix (MatrixParallel DIM2) Bool -> FocusedMatrix (MatrixParallel DIM2) Bool #-}

gameOfLife' :: (Matrix i, MatrixShape i ~ DIM2) => i Bool -> i Bool
gameOfLife' = mrun $ \img (Z :. x :. y) ->
    let n = sum [ if mindex img False (ix2 (x + dx) (y + dy)) then 1 else 0 | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0) ] in
        n == 3 || (mindex img False (ix2 x y) && n == 2)
{-# SPECIALIZE gameOfLife' :: MatrixVector DIM2 Bool -> MatrixVector DIM2 Bool #-}
{-# SPECIALIZE gameOfLife' :: MatrixArray DIM2 Bool -> MatrixArray DIM2 Bool #-}
{-# SPECIALIZE gameOfLife' :: MatrixParallel DIM2 Bool -> MatrixParallel DIM2 Bool #-}

takeiterate 0 _ _ = []
takeiterate !n f a = a : takeiterate (n - 1) f (f a)

toB :: Matrix i => i Word8 -> i Bool
toB = fmap (> 0.5) . toF
fromB :: Matrix i => i Bool -> i Word8
fromB = fromF . fmap (\v -> if v then 1 else 0)
toF :: Matrix i => i Word8 -> i Float
toF = fmap ((/ 256) . fromIntegral)
fromF :: Matrix i => i Float -> i Word8
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
        bgroup "MatrixVector" [
            bench "testsmall" $ nf (vmData . fromF . unfocus . autolight) $ focus $ toF testsmall,
            bench "testsmall'" $ nf (vmData . fromF . autolight') $ toF testsmall,
            bench "testbig" $ nf (vmData . fromF . unfocus . autolight) $ focus $ toF testbig,
            bench "testbig'" $ nf (vmData . fromF . autolight') $ toF testbig,
            bench "life0" $ nf (vmData . fromB . last . map unfocus . takeiterate 16 gameOfLife) $ focus $ toB life0,
            bench "life0'" $ nf (vmData . fromB . last . takeiterate 16 gameOfLife') $ toB life0
            ],
        bgroup "MatrixArray" [
            bench "testsmall" $ nf (vmData . fromF . fromArray . autolight') $ toArray $ toF testsmall,
            bench "testsmall" $ nf (vmData . fromF . fromArray . unfocus . autolight) $ focus $ toArray $ toF testsmall,
            bench "testbig" $ nf (vmData . fromF . fromArray . unfocus . autolight) $ focus $ toArray $ toF testbig,
            bench "testbig" $ nf (vmData . fromF . fromArray . autolight') $ toArray $ toF testbig,
            bench "life0" $ nf (vmData . fromB . last . map (fromArray . unfocus) . takeiterate 16 gameOfLife) $ focus $ toArray $ toB life0,
            bench "life0'" $ nf (vmData . fromB . last . map fromArray . takeiterate 16 gameOfLife') $ toArray $ toB life0
            ],
        bgroup "MatrixParallel" [
            bench "testsmall" $ nf (vmData . fromF . fromParallel . unfocus . autolight) $ focus $ toParallel $ toF testsmall,
            bench "testsmall'" $ nf (vmData . fromF . fromParallel . autolight') $ toParallel $ toF testsmall,
            bench "testbig" $ nf (vmData . fromF . fromParallel . unfocus . autolight) $ focus $ toParallel $ toF testbig,
            bench "testbig'" $ nf (vmData . fromF . fromParallel . autolight') $ toParallel $ toF testbig,
            bench "life0" $ nf (vmData . fromB . last . map (fromParallel . unfocus) . takeiterate 16 gameOfLife) $ focus $ toParallel $ toB life0,
            bench "life0'" $ nf (vmData . fromB . last . map fromParallel . takeiterate 16 gameOfLife') $ toParallel $ toB life0
            ]
        ]

