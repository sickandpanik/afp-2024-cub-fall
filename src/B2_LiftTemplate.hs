{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module B2_LiftTemplate (makeLiftA, makeLiftAUntyped) where

import Language.Haskell.TH
import Control.Monad


liftAName :: Show a => a -> Name
liftAName n = mkName $ "liftA" ++ show n -- liftAn, mkName - spliced exactly, can overlap


makeLiftABody :: Name -> [Name] -> Q Exp
-- (((((pure f) <*> x0) <*> x1) <*> ...) <*> xn)
makeLiftABody f xs = foldl (\a b -> [| $a <*> $(varE b) |]) [| pure $(varE f) |] xs

makeLiftADec :: Int-> Q Dec
makeLiftADec n = do
    -- newName - may be modified internally, can't be shadowed by later names
    f <- newName "f" -- f
    xs <- replicateM n (newName "x") -- x0, x1, ..., xn, 
    funD (liftAName n) [clause (varP f:(varP <$> xs)) (normalB $ makeLiftABody f xs) []]


makeLiftAUntyped :: Int -> Q [Dec]
makeLiftAUntyped n = return <$> makeLiftADec n























makeLiftASig :: Int -> Q Dec
makeLiftASig n = do
    let app = mkName "app"
    let y = mkName "y"
    xs <- replicateM n (newName "x")

    -- (x0 -> (x1 -> (... -> (xn -> y))))
    let fIn = func (varT <$> xs) (varT y)
    -- (app x0 -> (app x1 -> (... (app xn -> app y))))
    let fOut = func ((varT app `appT`) . varT <$> xs) (varT app `appT` varT y)
    sigD (liftAName n) -- liftAn :: 
            (
                forallT ([tv app, tv y] ++ (tv <$> xs)) -- forall app y x0 x1 ... xn.
                (cxt [conT ''Applicative `appT` varT app]) -- (Applicative app) =>
                [t| $fIn -> $fOut |] -- (x0 -> x1 -> ... -> xn -> y) -> (app x0 -> app x1 -> ... -> app xn -> app y)
            )
    where
        tv x = PlainTV x specifiedSpec
        -- (x0 -> (x1 -> ()... -> (xn -> y))))
        func xs y = foldr (\a b -> [t| $a -> $b |]) y xs

makeLiftA :: Int -> Q [Dec]
makeLiftA n = sequence [makeLiftASig n, makeLiftADec n]