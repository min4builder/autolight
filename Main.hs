{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Main where

import Data.Array.Accelerate as A (Word8, constant, toList, unit, use)
import Data.Array.Accelerate.LLVM.Native (run1)
import Data.Array.Accelerate.IO.Data.Vector.Unboxed as A (fromUnboxed, toUnboxed)
import Data.Array.Repa as R (Z(..), (:.)(..), fromUnboxed, toUnboxed)
import Data.Massiv.Array as M (Array, Comp(Par), Ix2((:.)), Sz(Sz), U, compute, fromList, resize')
import Data.Vector as V (toList)
import Data.Vector.Generic (convert)
import MatrixLoad
import qualified MatrixVector as MV
import qualified ComonadVector as CV
import qualified StencilsVector as SV
import qualified MatrixRepa as MR
import qualified ComonadRepa as CR
import qualified StencilsRepa as SR
import qualified MatrixMassiv as MM
import qualified ComonadMassiv as CM
import qualified StencilsMassiv as SM
import qualified MatrixAccelerate as MA
import qualified ComonadAccelerate as CA
import qualified StencilsAccelerate as SA

import Criterion.Main

times 0 f !a = a
times !n f !a = times (n - 1) f (f a)

toF :: MV.Matrix () sh Word8 -> MV.Matrix () sh Float
toF = MV.mmap ((/ 256) . fromIntegral)
toB :: MV.Matrix () sh Word8 -> MV.Matrix () sh Bool
toB = MV.mmap (> 0.5) . toF
toB' :: MV.Matrix () sh Word8 -> MV.Matrix () sh Word8
toB' = MV.mmap (\v -> if v > 127 then 1 else 0)
fromB = MA.mmap (\v -> MA.ifThenElse v (1 :: MA.Exp Int) 0)
clamp a b c
    | c < a = a
    | c > b = b
    | otherwise = c

toRepa (MV.Matrix sh v) = MR.Matrix sh $ R.fromUnboxed (Z R.:. MR.toLength sh) $ convert v
fromRepa d = MV.Matrix sh $ convert $ R.toUnboxed v
    where (MR.Matrix sh !v) = MR.evaluate d
toMassiv' :: MM.Unbox a => MV.Matrix () MV.Dim2 a -> Array U Ix2 a
toMassiv' (MV.Matrix (w, h) v) = resize' (Sz (w M.:. h)) $ fromList Par $ V.toList v
toMassiv :: MM.Unbox a => MV.Matrix () MV.Dim2 a -> MM.Matrix U MM.Dim2 a
toMassiv (MV.Matrix (w, h) v) = MM.Matrix (w, h) $ fromList Par $ V.toList v
runAccelerate f (MV.Matrix sh _) = \(MV.Matrix sh v) -> MV.Matrix sh $ convert $ A.toUnboxed $ f' $ A.fromUnboxed $ convert v
    where f' = run1 ((\(MA.Matrix sh v) -> v) . f . MA.Matrix sh)

vmData (MV.Matrix sh v) = v
mmData :: (MM.Load r MM.Ix1 a, MM.Unbox a) => MM.Matrix r sh a -> Array U MM.Ix1 a
mmData (MM.Matrix sh v) = compute v

main = do
    testsmall <- readImage "testsmall.png"
    testbig <- readImage "test.png"
    life0 <- readImage "life0.png"
    defaultMain [
        bgroup "Vector" [
--            bench "testsmall" $ nf (vmData . CV.unfocus . SV.autolight) $ CV.focus $ toF testsmall,
--            bench "testsmall'" $ nf (vmData . SV.autolight') $ toF testsmall,
            bench "testbig" $ nf (vmData . CV.unfocus . SV.autolight) $ CV.focus $ toF testbig,
            bench "testbig'" $ nf (vmData . SV.autolight') $ toF testbig,
            bench "life0" $ nf (vmData . CV.unfocus . times 16 SV.gameOfLife) $ CV.focus $ toB life0,
            bench "life0'" $ nf (vmData . times 16 SV.gameOfLife') $ toB life0
            ],
        bgroup "Repa" [
--            bench "testsmall" $ nf (vmData . fromRepa . CR.unfocus . SR.autolight) $ CR.focus $ MR.mresult $ toRepa $ toF testsmall,
--            bench "testsmall'" $ nf (vmData . fromRepa . SR.autolight') $ MR.mresult $ toRepa $ toF testsmall,
            bench "testbig" $ nf (vmData . fromRepa . CR.unfocus . SR.autolight) $ CR.focus $ MR.mresult $ toRepa $ toF testbig,
            bench "testbig'" $ nf (vmData . fromRepa . SR.autolight') $ MR.mresult $ toRepa $ toF testbig,
            bench "life0" $ nf (vmData . fromRepa . CR.unfocus . times 16 SR.gameOfLife) $ CR.focus $ MR.mresult $ toRepa $ toB life0,
            bench "life0'" $ nf (vmData . fromRepa . times 16 SR.gameOfLife') $ MR.mresult $ toRepa $ toB life0
            ],
        bgroup "Massiv" [
--            bench "testsmall" $ whnf (mmData . CM.unfocus . SM.autolight) $ CM.focus $ toMassiv $ toF testsmall,
--            bench "testsmall'" $ whnf SM.autolight' $ toMassiv' $ toF testsmall,
            bench "testbig" $ whnf (mmData . CM.unfocus . SM.autolight) $ CM.focus $ toMassiv $ toF testbig,
            bench "testbig'" $ whnf (SM.autolight') $ toMassiv' $ toF testbig,
            bench "life0" $ whnf (mmData . CM.unfocus . times 16 SM.gameOfLife) $ CM.focus $ MM.mresult $ toMassiv $ toB life0,
            bench "life0'" $ whnf (times 16 (compute . SM.gameOfLife')) $ toMassiv' $ toB life0
            ],
        bgroup "Accelerate" [
--            bench "testsmall" $ nf (vmData . runAccelerate (CA.unfocus . SA.autolight . CA.focus) testsmall) $ toF testsmall,
--            bench "testsmall'" $ nf (vmData . runAccelerate SA.autolight' testsmall) $ toF testsmall,
            bench "testbig" $ nf (vmData . runAccelerate (CA.unfocus . SA.autolight . CA.focus) testbig) $ toF testbig,
            bench "testbig'" $ nf (vmData . runAccelerate SA.autolight' testbig) $ toF testbig,
            bench "life0" $ nf (vmData . runAccelerate (CA.unfocus . times 16 SA.gameOfLife . CA.focus) life0) $ toB' life0,
            bench "life0'" $ nf (vmData . runAccelerate (times 16 SA.gameOfLife') life0) $ toB' life0
            ]
        ]

