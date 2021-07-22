{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Main where

import Data.Array.Repa (D)
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Array.Repa.Repr.Vector --(V, toVector)
import Data.List (foldl')
import Data.Word (Word8)
import MatrixComonad as MC

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

gradient :: (Matrix m r DIM2 p, Matrix m (MResult m) DIM2 (p, p), Num p) =>
            FocusedMatrix m r DIM2 p -> FocusedMatrix m (MResult m) DIM2 (p, p)
gradient = extend $ \img ->
    let v = extract img in (index img v (ix2 1 0) - v, index img v (ix2 0 1) - v)

gradient' :: (Matrix m r DIM2 p, Matrix m (MResult m) DIM2 (p, p), Num p) =>
             m r DIM2 p -> m (MResult m) DIM2 (p, p)
gradient' = mrun $ \img (Z :. x :. y) ->
    let v = mindex img undefined (ix2 x y) in (mindex img v (ix2 (x + 1) y) - v, mindex img v (ix2 x (y + 1)) - v)

distance r = extend $ \img ->
    minimum (r : [ sqrt (x*x + y*y) | x <- [-r .. r], y <- [-r .. r],
        index img False (ix2 (round x) (round y)) ])

distance' r = mrun $ \img (Z :. x :. y) ->
    minimum (r : [ sqrt (x'*x' + y'*y') | x' <- [(fromIntegral x) - r .. (fromIntegral x) + r], y' <- [(fromIntegral y) - r .. (fromIntegral y) + r],
        mindex img False (ix2 (round x') (round y')) ])

autolight img = MC.zipWith (*) img $ gaussianBlur 1 shadow
    where (Z :. w :. h) = msize $ unfocus img
          blurr = minimum [fromIntegral w, fromIntegral h] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur blurr img
          delta = MC.map (\(dx, dy) -> lx*dx + ly*dy) $ gradient blurry

          dist = distance distr $ MC.map (< 0.5) img
          mdist = MC.map (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
          shadow = MC.map ((+ 0.8) . (* 0.2) . signum) $ MC.zipWith (+) delta mdist

autolight' img = mzipWith (*) img $ gaussianBlur' 1 shadow
    where (Z :. w :. h) = msize img
          blurr = minimum [fromIntegral w, fromIntegral h] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur' blurr img
          delta = mmap (\(dx, dy) -> lx*dx + ly*dy) $ gradient' blurry

          dist = distance' distr $ mmap (< 0.5) img
          mdist = mmap (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
          shadow = mmap ((+ 0.8) . (* 0.2) . signum) $ mzipWith (+) delta mdist

gameOfLife :: Matrix m (MResult m) DIM2 Bool => FocusedMatrix m (MResult m) DIM2 Bool -> FocusedMatrix m (MResult m) DIM2 Bool
gameOfLife = extend $ \img ->
    let n = sum [ if index img False (ix2 x y) then 1 else 0 | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0) ] in
        n == 3 || (extract img && n == 2)

gameOfLife' :: Matrix m (MResult m) DIM2 Bool => m (MResult m) DIM2 Bool -> m (MResult m) DIM2 Bool
gameOfLife' = mrun $ \img (Z :. x :. y) ->
    let n = sum [ if mindex img False (ix2 (x + dx) (y + dy)) then 1 else 0 | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0) ] in
        n == 3 || (mindex img False (ix2 x y) && n == 2)

takeiterate 0 _ _ = []
takeiterate !n f a = a : takeiterate (n - 1) f (f a)

toF :: (Matrix m r sh Word8, Matrix m (MResult m) sh Float) =>
       m r sh Word8 -> m (MResult m) sh Float
toF = mmap ((/ 256) . fromIntegral)
fromF :: (Matrix m r sh Float, Matrix m (MResult m) sh Word8) =>
         m r sh Float -> m (MResult m) sh Word8
fromF = mmap (toEnum . round . (* 255) . clamp 0 1)
toB = mmap (> 0.5) . toF
fromB = fromF . mmap (\v -> if v then 1 else 0)
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
            bench "life0" $ nf (vmData . fromB . last . Prelude.map unfocus . takeiterate 16 gameOfLife) $ focus $ toB life0,
            bench "life0'" $ nf (vmData . fromB . last . takeiterate 16 gameOfLife') $ toB life0
            ],
        bgroup "MatrixArray" [
            bench "testsmall" $ nf (vmData . fromF . fromArray . autolight') $ toArray $ toF testsmall,
            bench "testsmall" $ nf (vmData . fromF . fromArray . unfocus . autolight) $ focus $ toArray $ toF testsmall,
            bench "testbig" $ nf (vmData . fromF . fromArray . unfocus . autolight) $ focus $ toArray $ toF testbig,
            bench "testbig" $ nf (vmData . fromF . fromArray . autolight') $ toArray $ toF testbig,
            bench "life0" $ nf (vmData . fromB . last . Prelude.map (fromArray . unfocus) . takeiterate 16 gameOfLife) $ focus $ mresult $ toArray $ toB life0,
            bench "life0'" $ nf (vmData . fromB . last . Prelude.map fromArray . takeiterate 16 gameOfLife') $ mresult $ toArray $ toB life0
            ],
        bgroup "MatrixParallel" [
            bench "testsmall" $ nf (vmData . fromF . fromParallel . unfocus . autolight) $ focus $ toParallel $ toF testsmall,
            bench "testsmall'" $ nf (vmData . fromF . fromParallel . autolight') $ toParallel $ toF testsmall,
            bench "testbig" $ nf (vmData . fromF . fromParallel . unfocus . autolight) $ focus $ toParallel $ toF testbig,
            bench "testbig'" $ nf (vmData . fromF . fromParallel . autolight') $ toParallel $ toF testbig,
            bench "life0" $ nf (vmData . fromB . last . Prelude.map (fromParallel . unfocus) . takeiterate 16 gameOfLife) $ focus $ mresult $ toParallel $ toB life0,
            bench "life0'" $ nf (vmData . fromB . last . Prelude.map fromParallel . takeiterate 16 gameOfLife') $ mresult $ toParallel $ toB life0
            ]
        ]

