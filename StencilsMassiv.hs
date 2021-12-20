{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module StencilsMassiv where

import Data.Default (Default, def)
import Data.List (foldl')
import qualified Data.Massiv.Array as M
import Data.Massiv.Array (Ix2((:.)))
import qualified Data.Massiv.Array.Stencil as M
import Data.Vector.Unboxed (Unbox)
import Data.Word (Word8)
import MatrixMassiv
import ComonadMassiv as MC

instance Default Bool where
    def = False

mget m p = if minside m p then mindex m p else def
get m p = if inside m p then index m p else def

compute :: (Load r sh a, Unbox a) => M.Array r sh a -> M.Array M.U sh a
compute = M.compute

{-# INLINEABLE gaussianBlur #-}
gaussianBlur r = vertical . horizontal
    where add !a !b = a + b
          horizontal = extend $ \img -> foldl' add 0 [ gauss (fromIntegral x) * get img (x, 0) | x <- [-3*(round r) .. 3*(round r)] ]
          vertical = extend $ \img -> foldl' add 0 [ gauss (fromIntegral y) * get img (0, y) | y <- [-3*(round r) .. 3*(round r)] ]
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

{-# INLINEABLE gaussianBlur' #-}
gaussianBlur' r' = vertical . compute . horizontal
    where add !a !b = a + b
          horizontal = M.mapStencil (M.Fill 0) $ M.makeStencil (M.Sz (6*r+3 :. 1)) (3*r :. 0) $ \get ->
              foldl' add 0 [ gauss (fromIntegral x) * get (x :. 0) | x <- [-3*r .. 3*r] ]
          vertical = M.mapStencil (M.Fill 0) $ M.makeStencil (M.Sz (1 :. 6*r+3)) (0 :. 3*r) $ \get ->
              foldl' add 0 [ gauss (fromIntegral y) * get (0 :. y) | y <- [-3*r .. 3*r] ]
          gauss n = exp (-(n**2 / (2 * (fromIntegral r)**2))) / sqrt (2 * pi * (fromIntegral r)**2)
          r = round r'

{-# INLINEABLE gradient #-}
gradient :: (Default a, Num a, Source r Ix1 a, Unbox a) => Focused r Dim2 a -> Focused MResult Dim2 (a, a)
gradient = extend $ \img ->
    let v = extract img in (get img (1, 0) - v, get img (0, 1) - v)

{-# INLINEABLE gradient' #-}
--gradient' :: (Default a, Num a, Source r Ix1 a, Unbox a) => M.Array r Dim2 a -> Matrix MResult Dim2 (a, a)
gradient' = M.mapStencil (M.Fill 0) $ M.makeStencil (M.Sz (2 :. 2)) (0 :. 0) $ \get ->
    let v = get (0 :. 0) in (get (1 :. 0) - v, get (0 :. 1) - v)

{-# INLINEABLE distance #-}
distance r = extend $ \img ->
    minimum (r : [ sqrt (x*x + y*y) | x <- [-r .. r], y <- [-r .. r],
        get img (round x, round y) ])

{-# INLINEABLE distance' #-}
distance' r = M.mapStencil (M.Fill False) $ M.makeStencil (M.Sz (round r * 2 + 3 :. round r * 2 + 3)) (round r :. round r) $ \get ->
    minimum (r : [ sqrt (x*x + y*y) | x <- [-r .. r], y <- [-r .. r],
        get (round x :. round y) ])

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
autolight' img = M.zipWith (*) img $ compute $ gaussianBlur' 1 $ compute shadow
    where (M.Sz (w :. h)) = M.size img
          blurr = minimum [fromIntegral w, fromIntegral h :: Float] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur' blurr img
          delta = M.map (\(dx, dy) -> lx*dx + ly*dy) $ compute $ gradient' $ compute blurry

          dist = distance' distr $ compute $ M.map (< 0.5) img
          mdist = M.map (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) $ compute dist
          shadow = M.map ((+ 0.8) . (* 0.2) . signum) $ M.zipWith (+) delta mdist

{-# INLINEABLE gameOfLife #-}
gameOfLife :: Source r Ix1 Bool => Focused r Dim2 Bool -> Focused MResult Dim2 Bool
gameOfLife = extend $ \img ->
    let n = sum [ if get img (x, y) then 1 else 0 | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0) ] :: Int in
        n == 3 || (extract img && n == 2)

{-# INLINEABLE gameOfLife' #-}
gameOfLife' :: M.Manifest r M.Ix2 Bool => M.Array r M.Ix2 Bool -> M.Array M.DW M.Ix2 Bool
gameOfLife' = M.mapStencil (M.Fill False) $ M.makeStencil (M.Sz (3 :. 3)) (1 :. 1) $ \get ->
    let n = sum [ if get (dx :. dy) then 1 else 0 | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0) ] :: Int in
        n == 3 || (get (0 :. 0) && n == 2)

