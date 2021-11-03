{-# LANGUAGE BangPatterns, FlexibleContexts, GADTs #-}
module Main where

import Data.Array.Repa (D, U)
import Data.Default (Default, def)
import Data.List (foldl')
import qualified Data.Massiv.Array as M (Array, Ix1)
import Data.Vector (Vector)
import Data.Vector.Unboxed (Unbox)
import Data.Word (Word8)
import Matrix
import MatrixLoad
import MatrixComonad as MC

import Criterion.Main

instance Default Bool where
    def = False

mget m p = if minside m p then mindex m p else def
get m p = if inside m p then index m p else def

gaussianBlur r = vertical . horizontal
    where add !a !b = a + b
          horizontal = extend $ \img -> foldl' add 0 [ gauss (fromIntegral x) * get img (x :. 0) | x <- [-3*(round r) .. 3*(round r)] ]
          vertical = extend $ \img -> foldl' add 0 [ gauss (fromIntegral y) * get img (0 :. y) | y <- [-3*(round r) .. 3*(round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

gaussianBlur' r = vertical . horizontal
    where add !a !b = a + b
          horizontal = mrun $ \img (x :. y) ->
              foldl' add 0 [ gauss (fromIntegral (x' - x)) * mget img (x' :. y) | x' <- [x - 3*(round r) .. x + 3*(round r)] ]
          vertical = mrun $ \img (x :. y) ->
              foldl' add 0 [ gauss (fromIntegral (y' - y)) * mget img (x :. y') | y' <- [y - 3*(round r) .. y + 3*(round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

gradient :: (Default a, Num a, Unbox a) => Focused r Dim2 a -> Focused (MResult r) Dim2 (a, a)
gradient = extend $ \img ->
    let v = extract img in (get img (1 :. 0) - v, get img (0 :. 1) - v)

gradient' :: (Default a, Num a, Unbox a) => Matrix r Dim2 a -> Matrix (MResult r) Dim2 (a, a)
gradient' = mrun $ \img (x :. y) ->
    let v = mget img (x :. y) in (mget img ((x + 1) :. y) - v, mget img (x :. (y + 1)) - v)

distance r = extend $ \img ->
    minimum (r : [ sqrt (x*x + y*y) | x <- [-r .. r], y <- [-r .. r],
        get img (round x :. round y) ])

distance' r = mrun $ \img (x :. y) ->
    minimum (r : [ sqrt (x'*x' + y'*y') | x' <- [(fromIntegral x) - r .. (fromIntegral x) + r], y' <- [(fromIntegral y) - r .. (fromIntegral y) + r],
        mget img (round x' :. round y') ])

autolight img = MC.zipWith (*) img $ gaussianBlur 1 shadow
    where (w :. h) = msize $ unfocus img
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
    where (w :. h) = msize img
          blurr = minimum [fromIntegral w, fromIntegral h] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur' blurr img
          delta = mmap (\(dx, dy) -> lx*dx + ly*dy) $ gradient' blurry

          dist = distance' distr $ mmap (< 0.5) img
          mdist = mmap (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
          shadow = mmap ((+ 0.8) . (* 0.2) . signum) $ mzipWith (+) delta mdist

gameOfLife = extend $ \img ->
    let n = sum [ if get img (x :. y) then 1 else 0 | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0) ] in
        n == 3 || (extract img && n == 2)

gameOfLife' = mrun $ \img (x :. y) ->
    let n = sum [ if mget img ((x + dx) :. (y + dy)) then 1 else 0 | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0) ] in
        n == 3 || (mget img (x :. y) && n == 2)

times 0 f !a = a
times !n f !a = times (n - 1) f (f a)

toF :: Matrix r sh Word8 -> Matrix (MResult r) sh Float
toF = mmap ((/ 256) . fromIntegral)
fromF :: Matrix r sh Float -> Matrix (MResult r) sh Word8
fromF = mmap (toEnum . round . (* 255) . clamp 0 1)
toB = mmap (> 0.5) . toF
fromB = fromF . mmap (\v -> if v then 1 else 0)
clamp a b c
    | c < a = a
    | c > b = b
    | otherwise = c

vmData :: Matrix Vector sh a -> Vector a
vmData (MatrixVector sh v) = v
mmData :: Matrix (M.Array r M.Ix1) sh a -> M.Array r M.Ix1 a
mmData (MatrixMassiv sh v) = v

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
            bench "life0" $ nf (vmData . fromB . unfocus . times 16 gameOfLife) $ focus $ toB life0,
            bench "life0'" $ nf (vmData . fromB . times 16 gameOfLife') $ toB life0
            ],
        bgroup "MatrixParallel" [
            bench "testsmall" $ nf (vmData . fromF . fromParallel . unfocus . autolight) $ focus $ mresult $ toParallel $ toF testsmall,
            bench "testsmall'" $ nf (vmData . fromF . fromParallel . autolight') $ mresult $ toParallel $ toF testsmall,
            bench "testbig" $ nf (vmData . fromF . fromParallel . unfocus . autolight) $ focus $ mresult $ toParallel $ toF testbig,
            bench "testbig'" $ nf (vmData . fromF . fromParallel . autolight') $ mresult $ toParallel $ toF testbig,
            bench "life0" $ nf (vmData . fromB . fromParallel . unfocus . times 16 gameOfLife) $ focus $ mresult $ toParallel $ toB life0,
            bench "life0'" $ nf (vmData . fromB . fromParallel . times 16 gameOfLife') $ mresult $ toParallel $ toB life0
            ],
        bgroup "MatrixMassiv" [
            bench "testsmall" $ whnf (mmData . fromF . autolight') $ toMassiv $ toF testsmall,
            bench "testsmall" $ whnf (mmData . fromF . unfocus . autolight) $ focus $ toMassiv $ toF testsmall,
            bench "testbig" $ whnf (mmData . fromF . unfocus . autolight) $ focus $ toMassiv $ toF testbig,
            bench "testbig" $ whnf (mmData . fromF . autolight') $ toMassiv $ toF testbig,
            bench "life0" $ whnf (mmData . fromB . unfocus . times 16 gameOfLife) $ focus $ mresult $ toMassiv $ toB life0,
            bench "life0'" $ whnf (mmData . fromB . times 16 gameOfLife') $ mresult $ toMassiv $ toB life0
            ]
        ]

