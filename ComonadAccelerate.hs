{-# LANGUAGE FlexibleContexts, RebindableSyntax, TypeFamilies #-}
module ComonadAccelerate (
    Focused, extract, extend, focus, index, ComonadAccelerate.inside, ComonadAccelerate.map, unfocus, ComonadAccelerate.zipWith
) where

import MatrixAccelerate

data Focused r sh a = Focused {
    unfocus :: Matrix r sh a,
    focusCoordinates :: Exp sh }

focus :: Shape (Exp sh) => Matrix r sh a -> Focused r sh a
focus mat = Focused mat zero

zipWith :: (Elt a, Elt b, Elt c, Shape sh) => (Exp a -> Exp b -> Exp c) -> Focused r sh a -> Focused s sh b -> Focused (MResult Matrix) sh c
zipWith f (Focused a _) (Focused b c) = Focused (mzipWith f a b) c

map :: (Elt a, Elt b) => (Exp a -> Exp b) -> Focused r sh a -> Focused (MResult Matrix) sh b
map f (Focused mat c) = Focused (mmap f mat) c

extract :: (Elt a, Elt sh, Shape (Exp sh), SInt (Exp sh) ~ Exp Int) => Focused r sh a -> Exp a
extract (Focused mat p) = mindex mat p

extend :: (Elt a, Elt b, Elt sh, Shape sh, Shape (Exp sh), SInt sh ~ Int, SInt (Exp sh) ~ Exp Int) => (Focused (MNormal Matrix) sh a -> Exp b) -> Focused r sh a -> Focused (MResult Matrix) sh b
extend f (Focused mat p) = Focused (mrun (\mat p -> f $ Focused mat p) mat) p

index :: (Elt a, Elt sh, Shape (Exp sh), SInt (Exp sh) ~ Exp Int) => Focused r sh a -> Exp sh -> Exp a
index (Focused mat fp) p = mindex mat (fp `off` p)
{-# INLINEABLE index #-}

inside :: (Elt sh, Shape (Exp sh)) => Focused r sh a -> Exp sh -> Exp Bool
inside (Focused mat fp) p = minside mat (fp `off` p)
{-# INLINEABLE inside #-}

