{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module Shape where

class Shape sh where
    zero :: sh
    toIndex :: sh -> sh -> Int
    fromIndex :: sh -> Int -> sh
    toLength :: sh -> Int
    sinside :: sh -> sh -> Bool
    szip :: (Int -> Int -> Int) -> sh -> sh -> sh
    sall :: (Int -> Int -> Bool) -> sh -> sh -> Bool
    off :: sh -> sh -> sh
    off a b = szip (+) a b

type Dim1 = Int
instance Shape Int where
    zero = 0
    toIndex _ x = x
    fromIndex _ s = s
    toLength x = x
    sinside s x = 0 <= x && x < s
    szip = id
    sall = id
type Dim2 = (Int, Int)
instance Shape (Int, Int) where
    zero = (0, 0)
    toIndex (s, _) (x, y) = x + s * y
    fromIndex (x, _) s = (s `mod` x, s `div` x)
    toLength (x, y) = x * y
    sinside (s, t) (x, y) = 0 <= x && x < s && 0 <= y && y < t
    szip f (s, t) (x, y) = (f s x, f t y)
    sall f (s, t) (x, y) = f s x && f t y
type Dim3 = (Int, Int, Int)
instance Shape (Int, Int, Int) where
    zero = (0, 0, 0)
    toIndex (s, t, _) (x, y, z) = x + s * y + s * t * z
    fromIndex (x, y, _) s = (s `mod` x, s `div` x `mod` y, s `div` x `div` y)
    toLength (x, y, z) = x * y * z
    sinside (s, t, u) (x, y, z) = 0 <= x && x < s && 0 <= y && y < t && 0 <= z && z < u
    szip f (s, t, u) (x, y, z) = (f s x, f t y, f u z)
    sall f (s, t, u) (x, y, z) = f s x && f t y && f u z
type Dim4 = (Int, Int, Int, Int)
instance Shape (Int, Int, Int, Int) where
    zero = (0, 0, 0, 0)
    toIndex (s, t, u, _) (x, y, z, w) = x + s * y + s * t * z + s * t * u * w
    fromIndex (x, y, z, _) s = (s `mod` x, s `div` x `mod` y, s `div` x `div` y `mod` z, s `div` x `div` y `div` z)
    toLength (x, y, z, w) = x * y * z * w
    sinside (s, t, u, v) (x, y, z, w) = 0 <= x && x < s && 0 <= y && y < t && 0 <= z && z < u && 0 <= w && w < v
    szip f (s, t, u, v) (x, y, z, w) = (f s x, f t y, f u z, f v w)
    sall f (s, t, u, v) (x, y, z, w) = f s x && f t y && f u z && f v w
type Dim5 = (Int, Int, Int, Int, Int)
instance Shape (Int, Int, Int, Int, Int) where
    zero = (0, 0, 0, 0, 0)
    toIndex (s, t, u, v, _) (x, y, z, w, ww) = x + s * y + s * t * z + s * t * u * w + s * t * u * v * ww
    fromIndex (x, y, z, w, _) s = (s `mod` x, s `div` x `mod` y, s `div` x `div` y `mod` z, s `div` x `div` y `div` z `mod` w, s `div` x `div` y `div` z `div` w)
    toLength (x, y, z, w, ww) = x * y * z * w * ww
    sinside (s, t, u, v, q) (x, y, z, w, ww) = 0 <= x && x < s && 0 <= y && y < t && 0 <= z && z < u && 0 <= w && w < v && 0 <= ww && ww < q
    szip f (s, t, u, v, q) (x, y, z, w, ww) = (f s x, f t y, f u z, f v w, f q ww)
    sall f (s, t, u, v, q) (x, y, z, w, ww) = f s x && f t y && f u z && f v w && f q ww

type family MResult (m :: * -> * -> * -> *) :: *
type family MNormal (m :: * -> * -> * -> *) :: *

