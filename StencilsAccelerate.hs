{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, RebindableSyntax, TypeFamilies #-}
module StencilsAccelerate where

import Data.Default (Default, def)
import Data.List (foldl')
import Data.Vector.Unboxed (Unbox)
import MatrixAccelerate
import ComonadAccelerate as MC
import Prelude (Num, minimum, sum)
import qualified Prelude as P

instance Default (Exp Bool) where
    def = constant False

instance Default (Exp Float) where
    def = 0.0

mget m p = if minside m (lift p) then mindex m (lift p) else def
get m p = if inside m (lift p) then index m (lift p) else def

{-# INLINEABLE gaussianBlur #-}
gaussianBlur :: Float -> Focused r Dim2 Float -> Focused (MResult Matrix) Dim2 Float
gaussianBlur r = vertical . horizontal
    where add !a !b = a + b
          horizontal = extend $ \img -> foldl' add 0 [ constant (gauss $ P.fromIntegral x) * get img (x :: Int, 0 :: Int) | x <- [-3*(P.round r) .. 3*(P.round r)] ]
          vertical = extend $ \img -> foldl' add 0 [ constant (gauss $ P.fromIntegral y) * get img (0 :: Int, y :: Int) | y <- [-3*(P.round r) .. 3*(P.round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

{-# INLINEABLE gaussianBlur' #-}
gaussianBlur' :: Float -> Matrix r Dim2 Float -> Matrix (MResult Matrix) Dim2 Float
gaussianBlur' r = vertical . horizontal
    where add !a !b = a + b
          horizontal = mrun $ \img xy -> let (x, y) = unlift xy in
              foldl' add 0 [ constant (gauss $ P.fromIntegral x') * mget img (x + constant x', y :: Exp Int) | x' <- [-3*(P.round r) :: Int .. 3*(P.round r)] ]
          vertical = mrun $ \img xy -> let (x, y) = unlift xy in
              foldl' add 0 [ constant (gauss $ P.fromIntegral y') * mget img (x :: Exp Int, y + constant y') | y' <- [-3*(P.round r) :: Int .. 3*(P.round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

{-# INLINEABLE gradient #-}
gradient :: Focused r Dim2 Float -> Focused (MResult Matrix) Dim2 (Float, Float)
gradient = extend $ \img ->
    let v = extract img in lift (get img (1 :: Int, 0 :: Int) - v, get img (0 :: Int, 1 :: Int) - v)

{-# INLINEABLE gradient' #-}
gradient' :: Matrix r Dim2 Float -> Matrix (MResult Matrix) Dim2 (Float, Float)
gradient' = mrun $ \img xy -> let (x, y) = unlift xy in
    let v = mget img (x :: Exp Int, y :: Exp Int) in lift (mget img (x + 1, y) - v, mget img (x, y + 1) - v)

{-# INLINEABLE distance #-}
distance r = extend $ \img ->
    minimum (constant r : [ if get img (P.round x :: Int, P.round y :: Int) then constant $ P.sqrt (x*x + y*y) else 0
         | x <- [-r :: Float .. r], y <- [-r :: Float .. r] ])

{-# INLINEABLE distance' #-}
distance' r = mrun $ \img xy -> let (x, y) = unlift xy :: (Exp Int, Exp Int) in
    minimum (constant r : [ if mget img (x + constant (P.round x' :: Int), y + constant (P.round y' :: Int)) then constant $ P.sqrt (x'*x' + y'*y') else 0
         | x' <- [-r :: Float .. r], y' <- [-r :: Float .. r] ])

{-# INLINEABLE autolight #-}
autolight img = MC.zipWith (*) img $ gaussianBlur 1 shadow
    where (w, h) = msize $ unfocus img
          blurr = minimum [P.fromIntegral w :: Float, P.fromIntegral h :: Float] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur blurr img
          delta = MC.map (\d -> let (dx, dy) = unlift d in lx*dx + ly*dy) $ gradient blurry

          dist = distance distr $ MC.map (< 0.5) img
          mdist = MC.map (\v -> distw * ((1 / (1 + exp (-v * 6 / constant distr))) - 1)) dist
          shadow = MC.map ((+ 0.8) . (* 0.2) . signum) $ MC.zipWith (+) delta mdist

{-# INLINEABLE autolight' #-}
autolight' img = mzipWith (*) img $ gaussianBlur' 1 shadow
    where (w, h) = msize img
          blurr = minimum [P.fromIntegral w :: Float, P.fromIntegral h :: Float] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur' blurr img
          delta = mmap (\d -> let (dx, dy) = unlift d in lx*dx + ly*dy) $ gradient' blurry

          dist = distance' distr $ mmap (< 0.5) img
          mdist = mmap (\v -> distw * ((1 / (1 + exp (-v * 6 / constant distr))) - 1)) dist
          shadow = mmap ((+ 0.8) . (* 0.2) . signum) $ mzipWith (+) delta mdist

{-# INLINEABLE gameOfLife #-}
gameOfLife :: Focused r Dim2 Bool -> Focused (MResult Matrix) Dim2 Bool
gameOfLife = extend $ \img ->
    let n = sum [ if get img (x, y) then 1 :: Exp Int else 0 | x <- [-1 :: Int .. 1], y <- [-1 :: Int .. 1], (x, y) P./= (0, 0) ] in
        n == 3 || (extract img && n == 2)

{-# INLINEABLE gameOfLife' #-}
gameOfLife' :: Matrix r Dim2 Bool -> Matrix (MResult Matrix) Dim2 Bool
gameOfLife' = mrun $ \img xy -> let (x, y) = unlift xy :: (Exp Int, Exp Int) in
    let n = sum [ if mget img (x + constant dx, y + constant dy) then 1 :: Exp Int else 0 | dx <- [-1 :: Int .. 1], dy <- [-1 :: Int .. 1], (dx, dy) P./= (0, 0) ] in
        n == 3 || (mget img (x, y) && n == 2)

