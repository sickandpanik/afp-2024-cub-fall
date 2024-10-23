{-# LANGUAGE TemplateHaskell #-}
module B1_Lifts where
import Control.Applicative
import B2_LiftTemplate

f2 :: Int -> Int -> Int
f2 x y = x + y

-- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 f x y = (pure f) <*> x <*> y

type Z = ZipList Int

zz :: [a] -> ZipList a
zz = ZipList

uz :: ZipList a -> [a]
uz = getZipList

f2' :: Z -> Z -> Z
f2' = liftA2 f2



f3 :: Int -> Int -> Int -> Int
f3 x y z = x + y + z

-- liftA3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- liftA3 f x y z = (pure f) <*> x <*> y <*> z

f3' :: Z -> Z -> Z -> Z
f3' = liftA3 f3

f4 :: Int -> Int -> Int -> Int -> Int
f4 x y z w = x + y + z + w

-- liftA4 - N/A

$(makeLiftAUntyped 4)

f4' :: Z -> Z -> Z -> Z -> Z
f4' = liftA4 f4

$(makeLiftA 10)