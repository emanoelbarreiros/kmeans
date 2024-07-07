{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}

module Lib where


class Ord v => Vector v where
    distance :: v -> v -> Float
    centroid :: [v] -> v


instance Vector (Float, Float) where
    distance :: (Float, Float) -> (Float, Float) -> Float
    distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)

    centroid :: [(Float, Float)] -> (Float, Float)
    centroid l = (u/n, v/n)
                 where
                    (u,v) = foldr (\(a,b) (c,d) -> (a + c, b + d)) (0,0) l
                    n = fromIntegral $ length l


class Vector v => Vectorizable e v where
    toVector :: e -> v



instance Vectorizable (Float, Float) (Float, Float) where
    toVector :: (Float, Float) -> (Float, Float)
    toVector = id