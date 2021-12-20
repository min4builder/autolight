{-# LANGUAGE FlexibleContexts #-}
module ComonadMassiv (
    Focused, extract, extend, focus, index, inside, ComonadMassiv.map, unfocus, ComonadMassiv.zipWith
) where

import MatrixMassiv

data Focused r sh a = Focused {
    unfocus :: Matrix r sh a,
    focusCoordinates :: sh }

focus :: Shape sh => Matrix r sh a -> Focused r sh a
focus mat = Focused mat zero

zipWith :: (Eq sh, Source r Ix1 a, Source s Ix1 b) => (a -> b -> c) -> Focused r sh a -> Focused s sh b -> Focused MResult sh c
zipWith f (Focused a _) (Focused b c) = Focused (mzipWith f a b) c

map :: Source r Ix1 a => (a -> b) -> Focused r sh a -> Focused MResult sh b
map f (Focused mat c) = Focused (mmap f mat) c

extract :: (Shape sh, Unbox a) => Focused MNormal sh a -> a
extract (Focused mat p) = mindex mat p

extend :: (Shape sh, Source r Ix1 a, Unbox a) => (Focused MNormal sh a -> b) -> Focused r sh a -> Focused MResult sh b
extend f (Focused mat p) = Focused (mrun (\mat p -> f $ Focused mat p) mat) p

index :: (Shape sh, Unbox a) => Focused MNormal sh a -> sh -> a
index (Focused mat fp) p = mindex mat (fp `off` p)
{-# INLINEABLE index #-}

inside :: Shape sh => Focused r sh a -> sh -> Bool
inside (Focused mat fp) p = minside mat (fp `off` p)
{-# INLINEABLE inside #-}

