{-# LANGUAGE TemplateHaskell #-}
module C2_CurryExercise where 

import Language.Haskell.TH

-- task 1: Implement generalized curry splice (inferred types)

curryName :: Int -> Name
curryName n = mkName $ "curry" ++ show n

makeCurryBody :: Int -> Q Exp
makeCurryBody n = undefined

makeCurryDec :: Int -> Q Dec
makeCurryDec n = undefined

makeCurryUntyped :: Int -> Q [Dec]
makeCurryUntyped n = undefined


-- task 2: Add explicity signature to curry

makeCurrySig :: Int -> Q Dec
makeCurrySig = undefined

makeCurry :: Int -> Q [Dec]
makeCurry n = undefined



-- (bonus) task 3: Do the same for generalized uncurry