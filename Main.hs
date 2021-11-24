{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Main where

import Data.Array.Accelerate as A (Word8, constant, toList, unit, use)
import Data.Array.Accelerate.LLVM.Native (run)
import Data.Array.Accelerate.IO.Data.Vector.Unboxed as A (fromUnboxed, toUnboxed)
import Data.Array.Repa as R (Z(..), (:.)(..), fromUnboxed, toUnboxed)
import Data.Massiv.Array (Array, Comp(Par), U, compute, fromList)
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
toBa :: MA.Matrix () sh Word8 -> MA.Matrix () sh Bool
toBa = MA.mmap (MA.> 128)
fromB = MA.mmap (\v -> MA.ifThenElse v (1 :: MA.Exp Int) 0)
clamp a b c
    | c < a = a
    | c > b = b
    | otherwise = c

toRepa (MV.Matrix sh v) = MR.Matrix sh $ R.fromUnboxed (Z :. MR.toLength sh) $ convert v
fromRepa d = MV.Matrix sh $ convert $ R.toUnboxed v
    where (MR.Matrix sh !v) = MR.evaluate d
toMassiv :: MM.Unbox a => MV.Matrix () sh a -> MM.Matrix U sh a
toMassiv (MV.Matrix sh v) = MM.Matrix sh $ fromList Par $ V.toList v
toAccelerate (MV.Matrix (w, h) v) = MA.Matrix (constant w, constant h) $ use $ A.fromUnboxed $ convert v
fromAccelerate (MA.Matrix (w, h) v) = MV.Matrix (head $ A.toList $ run $ unit w, head $ A.toList $ run $ unit h) $ convert $ A.toUnboxed $ run v

vmData (MV.Matrix sh v) = v
mmData :: (MM.Load r MM.Ix1 a, MM.Unbox a) => MM.Matrix r sh a -> Array U MM.Ix1 a
mmData (MM.Matrix sh v) = compute v

main = do
    testsmall <- readImage "testsmall.png"
    testbig <- readImage "testbig.png"
    life0 <- readImage "life0.png"
    defaultMain [
        bgroup "Vector" [
            bench "testsmall" $ nf (vmData . CV.unfocus . SV.autolight) $ CV.focus $ toF testsmall,
            bench "testsmall'" $ nf (vmData . SV.autolight') $ toF testsmall,
            bench "testbig" $ nf (vmData . CV.unfocus . SV.autolight) $ CV.focus $ toF testbig,
            bench "testbig'" $ nf (vmData . SV.autolight') $ toF testbig,
            bench "life0" $ nf (vmData . CV.unfocus . times 16 SV.gameOfLife) $ CV.focus $ toB life0,
            bench "life0'" $ nf (vmData . times 16 SV.gameOfLife') $ toB life0
            ],
        bgroup "Repa" [
            bench "testsmall" $ nf (vmData . fromRepa . CR.unfocus . SR.autolight) $ CR.focus $ MR.mresult $ toRepa $ toF testsmall,
            bench "testsmall'" $ nf (vmData . fromRepa . SR.autolight') $ MR.mresult $ toRepa $ toF testsmall,
            bench "testbig" $ nf (vmData . fromRepa . CR.unfocus . SR.autolight) $ CR.focus $ MR.mresult $ toRepa $ toF testbig,
            bench "testbig'" $ nf (vmData . fromRepa . SR.autolight') $ MR.mresult $ toRepa $ toF testbig,
            bench "life0" $ nf (vmData . fromRepa . CR.unfocus . times 16 SR.gameOfLife) $ CR.focus $ MR.mresult $ toRepa $ toB life0,
            bench "life0'" $ nf (vmData . fromRepa . times 16 SR.gameOfLife') $ MR.mresult $ toRepa $ toB life0
            ],
        bgroup "Massiv" [
            bench "testsmall" $ whnf (mmData . CM.unfocus . SM.autolight) $ CM.focus $ toMassiv $ toF testsmall,
            bench "testsmall'" $ whnf (mmData . SM.autolight') $ toMassiv $ toF testsmall,
            bench "testbig" $ whnf (mmData . CM.unfocus . SM.autolight) $ CM.focus $ toMassiv $ toF testbig,
            bench "testbig'" $ whnf (mmData . SM.autolight') $ toMassiv $ toF testbig,
            bench "life0" $ whnf (mmData . CM.unfocus . times 16 SM.gameOfLife) $ CM.focus $ MM.mresult $ toMassiv $ toB life0,
            bench "life0'" $ whnf (mmData . times 16 SM.gameOfLife') $ MM.mresult $ toMassiv $ toB life0
            ],
        bgroup "Accelerate" [
            bench "testsmall" $ whnf (vmData . fromAccelerate . CA.unfocus . SA.autolight) $ CA.focus $ toAccelerate $ toF testsmall,
            bench "testsmall'" $ whnf (vmData . fromAccelerate . SA.autolight') $ toAccelerate $ toF testsmall,
            bench "testbig" $ whnf (vmData . fromAccelerate . CA.unfocus . SA.autolight) $ CA.focus $ toAccelerate $ toF testbig,
            bench "testbig'" $ whnf (vmData . fromAccelerate . SA.autolight') $ toAccelerate $ toF testbig,
            bench "life0" $ whnf (vmData . fromAccelerate . fromB . CA.unfocus . times 16 SA.gameOfLife) $ CA.focus $ MA.mresult $ toBa $ toAccelerate life0,
            bench "life0'" $ whnf (vmData . fromAccelerate . fromB . times 16 SA.gameOfLife') $ MA.mresult $ toBa $ toAccelerate life0
            ]
        ]

