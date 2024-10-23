{-# LANGUAGE TemplateHaskell #-}
module D2_EmbeddingExample where

import qualified D1_Embedding as E
import Data.Bifunctor (bimap)


$(E.embedProg "addo" E.addoPrg)

toTerm :: Int -> Term
toTerm 0 = Zero
toTerm n = Succ (toTerm (pred n))

fromTerm :: Term -> Int
fromTerm Zero = 0
fromTerm (Succ n) = succ (fromTerm n)

testAddoIIO :: Int -> Int -> Int
testAddoIIO x y = fromTerm $ head $ addoIIO (toTerm x) (toTerm y)

testAddoOOI :: Int -> [(Int, Int)]
testAddoOOI z = bimap fromTerm fromTerm <$> addoOOI (toTerm z)

