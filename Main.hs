{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}
module Main where

import Data.Array.Repa (D, U)
import Data.Array.Repa.Index
import Data.Default (Default, def)
import Data.List (foldl')
import Data.Word (Word8)
import MatrixComonad as MC

import Criterion.Main

instance Default Bool where
    def = False

mget m p = if minside m p then mindex m p else def
get m p = if inside m p then index m p else def

gaussianBlur r = vertical . horizontal
    where add !a !b = a + b
          horizontal = extend $ \img -> foldl' add 0 [ gauss (fromIntegral x) * get img (ix2 x 0) | x <- [-3*(round r) .. 3*(round r)] ]
          vertical = extend $ \img -> foldl' add 0 [ gauss (fromIntegral y) * get img (ix2 0 y) | y <- [-3*(round r) .. 3*(round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

gaussianBlur' r = vertical . horizontal
    where add !a !b = a + b
          horizontal = mrun $ \img (Z :. x :. y) ->
              foldl' add 0 [ gauss (fromIntegral (x' - x)) * mget img (ix2 x' y) | x' <- [x - 3*(round r) .. x + 3*(round r)] ]
          vertical = mrun $ \img (Z :. x :. y) ->
              foldl' add 0 [ gauss (fromIntegral (y' - y)) * mget img (ix2 x y') | y' <- [y - 3*(round r) .. y + 3*(round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

gradient :: (Matrix m r DIM2 p, Matrix m (MResult m) DIM2 (p, p), Num p, Default p) =>
            FocusedMatrix m r DIM2 p -> FocusedMatrix m (MResult m) DIM2 (p, p)
gradient = extend $ \img ->
    let v = extract img in (get img (ix2 1 0) - v, get img (ix2 0 1) - v)

gradient' :: (Matrix m r DIM2 p, Matrix m (MResult m) DIM2 (p, p), Num p, Default p) =>
             m r DIM2 p -> m (MResult m) DIM2 (p, p)
gradient' = mrun $ \img (Z :. x :. y) ->
    let v = mget img (ix2 x y) in (mget img (ix2 (x + 1) y) - v, mget img (ix2 x (y + 1)) - v)

distance r = extend $ \img ->
    minimum (r : [ sqrt (x*x + y*y) | x <- [-r .. r], y <- [-r .. r],
        get img (ix2 (round x) (round y)) ])

distance' r = mrun $ \img (Z :. x :. y) ->
    minimum (r : [ sqrt (x'*x' + y'*y') | x' <- [(fromIntegral x) - r .. (fromIntegral x) + r], y' <- [(fromIntegral y) - r .. (fromIntegral y) + r],
        mget img (ix2 (round x') (round y')) ])

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
{-# SPECIALIZE autolight :: FocusedMatrix MatrixVector () DIM2 Float -> FocusedMatrix MatrixVector () DIM2 Float #-}
{-# SPECIALIZE autolight :: FocusedMatrix MatrixArray U DIM2 Float -> FocusedMatrix MatrixArray D DIM2 Float #-}
{-# SPECIALIZE autolight :: FocusedMatrix MatrixParallel U DIM2 Float -> FocusedMatrix MatrixParallel D DIM2 Float #-}

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
{-# SPECIALIZE autolight' :: MatrixVector () DIM2 Float -> MatrixVector () DIM2 Float #-}
{-# SPECIALIZE autolight' :: MatrixArray U DIM2 Float -> MatrixArray D DIM2 Float #-}
{-# SPECIALIZE autolight' :: MatrixParallel U DIM2 Float -> MatrixParallel D DIM2 Float #-}

gameOfLife :: Matrix m (MResult m) DIM2 Bool => FocusedMatrix m (MResult m) DIM2 Bool -> FocusedMatrix m (MResult m) DIM2 Bool
gameOfLife = extend $ \img ->
    let n = sum [ if get img (ix2 x y) then 1 else 0 | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0) ] in
        n == 3 || (extract img && n == 2)
{-# SPECIALIZE gameOfLife :: FocusedMatrix MatrixVector () DIM2 Bool -> FocusedMatrix MatrixVector () DIM2 Bool #-}
{-# SPECIALIZE gameOfLife :: FocusedMatrix MatrixArray D DIM2 Bool -> FocusedMatrix MatrixArray D DIM2 Bool #-}
{-# SPECIALIZE gameOfLife :: FocusedMatrix MatrixParallel D DIM2 Bool -> FocusedMatrix MatrixParallel D DIM2 Bool #-}

gameOfLife' :: Matrix m (MResult m) DIM2 Bool => m (MResult m) DIM2 Bool -> m (MResult m) DIM2 Bool
gameOfLife' = mrun $ \img (Z :. x :. y) ->
    let n = sum [ if mget img (ix2 (x + dx) (y + dy)) then 1 else 0 | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0) ] in
        n == 3 || (mget img (ix2 x y) && n == 2)
{-# SPECIALIZE gameOfLife' :: MatrixVector () DIM2 Bool -> MatrixVector () DIM2 Bool #-}
{-# SPECIALIZE gameOfLife' :: MatrixArray D DIM2 Bool -> MatrixArray D DIM2 Bool #-}
{-# SPECIALIZE gameOfLife' :: MatrixParallel D DIM2 Bool -> MatrixParallel D DIM2 Bool #-}

times 0 f !a = a
times !n f !a = times (n - 1) f (f a)

toF :: (Matrix m r sh Word8, Matrix m (MResult m) sh Float) =>
       m r sh Word8 -> m (MResult m) sh Float
toF = mmap ((/ 256) . fromIntegral)
fromF :: (Matrix m r sh Float, Matrix m (MResult m) sh Word8) =>
         m r sh Float -> m (MResult m) sh Word8
fromF = mmap (toEnum . round . (* 255) . clamp 0 1)
toB :: (Matrix m r sh Word8, Matrix m (MResult m) sh Float, Matrix m (MResult m) sh Bool) =>
       m r sh Word8 -> m (MResult m) sh Bool
toB = mmap (> 0.5) . toF
fromB :: (Matrix m r sh Bool, Matrix m (MResult m) sh Float, Matrix m (MResult m) sh Word8) =>
         m r sh Bool -> m (MResult m) sh Word8
fromB = fromF . mmap (\v -> if v then 1 else 0)
clamp a b c
    | c < a = a
    | c > b = b
    | otherwise = c

main = do
--    testsmall <- readImage "testsmall.png"
--    testbig <- readImage "test.png"
    life0 <- readImage "life0.png"
    defaultMain [
        bgroup "MatrixVector" [
--            bench "testsmall" $ nf (vmData . fromF . unfocus . autolight) $ focus $ toF testsmall,
--            bench "testsmall'" $ nf (vmData . fromF . autolight') $ toF testsmall,
--            bench "testbig" $ nf (vmData . fromF . unfocus . autolight) $ focus $ toF testbig,
--            bench "testbig'" $ nf (vmData . fromF . autolight') $ toF testbig,
            bench "life0" $ nf (vmData . fromB . unfocus . times 16 gameOfLife) $ focus $ toB life0,
            bench "life0'" $ nf (vmData . fromB . times 16 gameOfLife') $ toB life0
            ],
        bgroup "MatrixArray" [
--            bench "testsmall" $ nf (vmData . fromF . fromArray . autolight') $ toArray $ toF testsmall,
--            bench "testsmall" $ nf (vmData . fromF . fromArray . unfocus . autolight) $ focus $ toArray $ toF testsmall,
--            bench "testbig" $ nf (vmData . fromF . fromArray . unfocus . autolight) $ focus $ toArray $ toF testbig,
--            bench "testbig" $ nf (vmData . fromF . fromArray . autolight') $ toArray $ toF testbig,
            bench "life0" $ nf (vmData . fromB . fromArray . unfocus . times 16 gameOfLife) $ focus $ mresult $ toArray $ toB life0,
            bench "life0'" $ nf (vmData . fromB . fromArray . times 16 gameOfLife') $ mresult $ toArray $ toB life0
            ],
        bgroup "MatrixParallel" [
--            bench "testsmall" $ nf (vmData . fromF . fromParallel . unfocus . autolight) $ focus $ toParallel $ toF testsmall,
--            bench "testsmall'" $ nf (vmData . fromF . fromParallel . autolight') $ toParallel $ toF testsmall,
--            bench "testbig" $ nf (vmData . fromF . fromParallel . unfocus . autolight) $ focus $ toParallel $ toF testbig,
--            bench "testbig'" $ nf (vmData . fromF . fromParallel . autolight') $ toParallel $ toF testbig,
            bench "life0" $ nf (vmData . fromB . fromParallel . unfocus . times 16 gameOfLife) $ focus $ mresult $ toParallel $ toB life0,
            bench "life0'" $ nf (vmData . fromB . fromParallel . times 16 gameOfLife') $ mresult $ toParallel $ toB life0
            ],
        bgroup "MatrixMassiv" [
--            bench "testsmall" $ nf (mmData . fromF . autolight') $ toMassiv $ toF testsmall,
--            bench "testsmall" $ nf (mmData . fromF . unfocus . autolight) $ focus $ toMassiv $ toF testsmall,
--            bench "testbig" $ nf (mmData . fromF . unfocus . autolight) $ focus $ toMassiv $ toF testbig,
--            bench "testbig" $ nf (mmData . fromF . autolight') $ toMassiv $ toF testbig,
            bench "life0" $ whnf (mmData . fromB . unfocus . times 16 gameOfLife) $ focus $ mresult $ toMassiv $ toB life0,
            bench "life0'" $ whnf (mmData . fromB . times 16 gameOfLife') $ mresult $ toMassiv $ toB life0
            ]
        ]

