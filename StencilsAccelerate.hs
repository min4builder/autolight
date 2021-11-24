{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, RebindableSyntax #-}
module StencilsAccelerate where

import Data.Default (Default, def)
import Data.List (foldl')
import Data.Word (Word8)
import MatrixAccelerate
import ComonadAccelerate as MC
import qualified Prelude as P

instance Default (Exp Bool) where
    def = constant False

instance Default (Exp Float) where
    def = constant 0.0

mget m p = if minside m p then mindex m p else def
get m p = if inside m p then index m p else def

sumRange (a, b) f = if a < b then f a + sumRange (a+1, b) f else 0

gaussianBlur r = vertical . horizontal
    where horizontal = extend $ \img -> sumRange (-3 * round r, 3 * round r) $ \x -> gauss (fromIntegral x) * get img (x, 0)
          vertical = extend $ \img -> sumRange (-3 * round r, 3 * round r) $ \y -> gauss (fromIntegral y) * get img (0, y)
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

gaussianBlur' r = vertical . horizontal
    where horizontal = mrun $ \img (x, y) -> sumRange (-3 * round r, 3 * round r) $ \x' -> gauss (fromIntegral x') * mget img (x + x', y)
          vertical = mrun $ \img (x, y) -> sumRange (-3 * round r, 3 * round r) $ \y' -> gauss (fromIntegral y') * mget img (x, y + y')
          gauss n = exp (-(n**2 / (2 * r**2))) / sqrt (2 * pi * r**2)

gradient :: Focused r Dim2 Float -> Focused (MResult Matrix) Dim2 (Float, Float)
gradient = extend $ \img ->
    let v = extract img in lift (get img (1, 0) - v, get img (0, 1) - v)

gradient' :: Matrix r Dim2 Float -> Matrix (MResult Matrix) Dim2 (Float, Float)
gradient' = mrun $ \img (x, y) ->
    let v = mget img (x, y) in lift (mget img (x + 1, y) - v, mget img (x, y + 1) - v)

distance r = extend $ \img -> minloop img (-r) (-r) r
    where min img x y m = if get img (round x, round y) && d < m then d else m
              where d = sqrt (x*x + y*y)
          minloop img x y m =
              if x < r then
                  minloop img (x+1) y (min img x y m)
              else if y < r then
                  minloop img (-r) (y+1) (min img x y m)
              else min img x y m

distance' r = mrun $ \img c -> minloop c img (-r) (-r) r
    where min (x, y) img x' y' m = if mget img (round x'+x, round y'+x) && d < m then d else m
              where d = sqrt (x'*x' + y'*y')
          minloop c img x y m =
              if x < r then
                  minloop c img (x+1) y (min c img x y m)
              else if y < r then
                  minloop c img (-r) (y+1) (min c img x y m)
              else min c img x y m


autolight img = zipWith (*) img $ gaussianBlur 1 shadow
    where (w, h) = msize $ unfocus img
          blurr = minimum [fromIntegral w, fromIntegral h] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur blurr img
          delta = map (\d -> let (dx, dy) = unlift d in lx*dx + ly*dy) $ gradient blurry

          dist = distance distr $ map (< 0.5) img
          mdist = map (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
          shadow = map ((+ 0.8) . (* 0.2) . signum) $ zipWith (+) delta mdist

autolight' img = mzipWith (*) img $ gaussianBlur' 1 shadow
    where (w, h) = msize img
          blurr = minimum [fromIntegral w, fromIntegral h] / 16
          distr = blurr / 2
          distw = 0.01
          (lx, ly) = (1, 1)

          blurry = gaussianBlur' blurr img
          delta = mmap (\d -> let (dx, dy) = unlift d in lx*dx + ly*dy) $ gradient' blurry

          dist = distance' distr $ mmap (< 0.5) img
          mdist = mmap (\v -> distw * ((1 / (1 + exp (-v * 6 / distr))) - 1)) dist
          shadow = mmap ((+ 0.8) . (* 0.2) . signum) $ mzipWith (+) delta mdist

gameOfLife :: Focused r Dim2 Bool -> Focused (MResult Matrix) Dim2 Bool
gameOfLife = extend $ \img ->
    let n = sum [ if lift (x, y) /= constant (0, 0) && get img (x, y) then 1 else 0 | x <- [-1 .. 1], y <- [-1 .. 1] ] :: Exp Int in
        n == 3 || (extract img && n == 2)

gameOfLife' :: Matrix r Dim2 Bool -> Matrix (MResult Matrix) Dim2 Bool
gameOfLife' = mrun $ \img (x, y) ->
    let n = sum [ if lift (dx, dy) /= constant (0, 0) && mget img (x + dx, y + dy) then 1 else 0 | dx <- [-1 .. 1], dy <- [-1 .. 1] ] :: Exp Int in
        n == 3 || (mget img (x, y) && n == 2)

