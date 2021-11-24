{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module StencilsRepa where

import Data.Default (Default, def)
import Data.List (foldl')
import Data.Vector.Unboxed (Unbox)
import Data.Word (Word8)
import MatrixRepa
import ComonadRepa as MC

instance Default Bool where
    def = False

mget m p = if minside m p then mindex m p else def
get m p = if inside m p then index m p else def

{-# INLINEABLE gaussianBlur #-}
gaussianBlur r = vertical . horizontal
    where add !a !b = a + b
          horizontal = extend $ \img -> foldl' add 0 [ gauss (fromIntegral x) * get img (x, 0) | x <- [-3*(round r) .. 3*(round r)] ]
          vertical = extend $ \img -> foldl' add 0 [ gauss (fromIntegral y) * get img (0, y) | y <- [-3*(round r) .. 3*(round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

{-# INLINEABLE gaussianBlur' #-}
gaussianBlur' r = vertical . horizontal
    where add !a !b = a + b
          horizontal = mrun $ \img (x, y) ->
              foldl' add 0 [ gauss (fromIntegral (x' - x)) * mget img (x', y) | x' <- [x - 3*(round r) .. x + 3*(round r)] ]
          vertical = mrun $ \img (x, y) ->
              foldl' add 0 [ gauss (fromIntegral (y' - y)) * mget img (x, y') | y' <- [y - 3*(round r) .. y + 3*(round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

{-# INLINEABLE gradient #-}
gradient :: (Default a, Evaluator r, Num a, Source r a, Unbox a) => Focused r Dim2 a -> Focused (MResult Matrix) Dim2 (a, a)
gradient = extend $ \img ->
    let v = extract img in (get img (1, 0) - v, get img (0, 1) - v)

{-# INLINEABLE gradient' #-}
gradient' :: (Default a, Evaluator r, Num a, Source r a, Unbox a) => Matrix r Dim2 a -> Matrix (MResult Matrix) Dim2 (a, a)
gradient' = mrun $ \img (x, y) ->
    let v = mget img (x, y) in (mget img (x + 1, y) - v, mget img (x, y + 1) - v)

{-# INLINEABLE distance #-}
distance r = extend $ \img ->
    minimum (r : [ sqrt (x*x + y*y) | x <- [-r .. r], y <- [-r .. r],
        get img (round x, round y) ])

{-# INLINEABLE distance' #-}
distance' r = mrun $ \img (x, y) ->
    minimum (r : [ sqrt (x'*x' + y'*y') | x' <- [(fromIntegral x) - r .. (fromIntegral x) + r], y' <- [(fromIntegral y) - r .. (fromIntegral y) + r],
        mget img (round x', round y') ])

{-# INLINEABLE autolight #-}
autolight img = MC.zipWith (*) img $ gaussianBlur 1 shadow
    where (w, h) = msize $ unfocus img
          blurr = minimum [fromIntegral w, fromIntegral h] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur blurr img
          delta = MC.map (\(dx, dy) -> lx*dx + ly*dy) $ gradient blurry

          dist = distance distr $ MC.map (< 0.5) img
          mdist = MC.map (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
          shadow = MC.map ((+ 0.8) . (* 0.2) . signum) $ MC.zipWith (+) delta mdist

{-# INLINEABLE autolight' #-}
autolight' img = mzipWith (*) img $ gaussianBlur' 1 shadow
    where (w, h) = msize img
          blurr = minimum [fromIntegral w, fromIntegral h] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur' blurr img
          delta = mmap (\(dx, dy) -> lx*dx + ly*dy) $ gradient' blurry

          dist = distance' distr $ mmap (< 0.5) img
          mdist = mmap (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
          shadow = mmap ((+ 0.8) . (* 0.2) . signum) $ mzipWith (+) delta mdist

{-# INLINEABLE gameOfLife #-}
gameOfLife :: (Evaluator r, Source r Bool) => Focused r Dim2 Bool -> Focused (MResult Matrix) Dim2 Bool
gameOfLife = extend $ \img ->
    let n = sum [ if get img (x, y) then 1 else 0 | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0) ] :: Int in
        n == 3 || (extract img && n == 2)

{-# INLINEABLE gameOfLife' #-}
gameOfLife' :: Evaluator r => Matrix r Dim2 Bool -> Matrix (MResult Matrix) Dim2 Bool
gameOfLife' = mrun $ \img (x, y) ->
    let n = sum [ if mget img (x + dx, y + dy) then 1 else 0 | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0) ] :: Int in
        n == 3 || (mget img (x, y) && n == 2)

