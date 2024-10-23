{-# LANGUAGE FunctionalDependencies, FlexibleInstances, TemplateHaskell #-}

module D1_Embedding where
import qualified Data.Char
import Language.Haskell.TH

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty, NonEmpty((:|)))
import Control.Monad (MonadPlus, guard, msum, mzero)
import Data.Functor (($>))

modifyFirstLetter :: (Char -> Char) -> String -> String
modifyFirstLetter _ "" = ""
modifyFirstLetter f (h:t) = f h : t

toLower :: String -> String
toLower = modifyFirstLetter Data.Char.toLower

toUpper :: String -> String
toUpper = modifyFirstLetter Data.Char.toUpper

newtype Var = NormalVar Int deriving (Show, Eq, Ord)

data Term = Var Var
          | Con String [Var] deriving (Show, Eq)

data Lang = Empty
          | Sum (NonEmpty Lang)
          | Bind (NonEmpty Lang) [Var]
          | Assn Term Term
          | Guard Var Term
          | Call String [Var] [Var]
          deriving (Show, Eq)

data Def = Def { name :: String, args :: [Var], body :: Lang } deriving (Show, Eq)

newtype ProgramTypes = ProgramTypes [(String, Int)] deriving (Show, Eq)

data Program = Program { types :: ProgramTypes, defs :: [Def], goal :: Maybe String } deriving (Show, Eq)

data ProgramDec = ProgramDec { decs :: [Dec], callGoal :: Maybe Exp }

type Error = String

outVars :: Lang -> [Term]
outVars Empty = []
outVars (Call _ _ ret) = Var <$> ret
outVars (Assn _ ret) = [ret]
outVars (Sum (x :| _)) = outVars x
outVars (Bind _ ret) = Var <$> ret
outVars (Guard _ _) = []

class Quotable a q | a -> q where
  toQuote :: a -> Q q

embedProg :: String -> Program -> Q [Dec]
embedProg n p = do
  (ProgramDec decs_ body_) <- toQuote p
  call <- callToDec n (return <$> body_)
  return $ decs_ ++ call

callToDec :: String -> Maybe (Q Exp) -> Q [Dec]
callToDec n (Just body_) = sequence [funD (mkName n) [clause [] (normalB body_) []]]
callToDec _ _ = return []

qname :: String -> Q Exp
qname n
      | null n = fail "Name cannot be empty"
      | otherwise = varE $ mkName n

pname :: String -> Q Pat
pname n
     | null n = fail "Var name cannot be empty"
     | otherwise = varP $ mkName n

qdef :: String -> Name
qdef = mkName

nameVar :: Var -> String
nameVar (NormalVar i) = 'x' : show i

qvar :: Var -> Q Exp
qvar = qname . nameVar

pvar :: Var -> Q Pat
pvar = pname . nameVar

pterm :: Term -> Q Pat
pterm (Var v) = pvar v
pterm (Con n args_) = conP (mkName n) (pvar <$> args_)

instance Quotable Term Exp where
  toQuote (Var v) = qvar v
  toQuote (Con v terms)
    | null v = fail "Constructor name cannot be empty"
    | otherwise = foldl appE (conE (mkName (toUpper v))) (qvar <$> terms)

instance Quotable Def [Dec] where
  toQuote d = sequence [
        sigD decName quoteSig,
        funD decName [quoteBody]
    ]
    where
      quoteBody :: Q Clause
      quoteBody = clause (pvar <$> args d) (normalB (toQuote (body d))) []

      quoteSig :: Q Type
      quoteSig = do
        let monad = mkName "m"
        let y = returnType (outVars (body d) $> term)
        forallT
            [PlainTV monad specifiedSpec]
            (cxt [conT ''MonadPlus `appT` varT monad, conT ''MonadFail `appT` varT monad])
            $ foldr (\a b -> [t| $a -> $b |]) (varT monad `appT` y) (args d $> term)

      returnType :: [Q Type] -> Q Type
      returnType [] = conT '()
      returnType [t] = t
      returnType ts = foldl appT (tupleT (length ts)) ts

      decName = mkName (name d)
      term = conT $ mkName "Term"

($:) :: Name -> [Q Exp] -> Q Exp
($:) = foldl appE . varE

instance Quotable Lang Exp where
  toQuote Empty = varE 'mzero
  toQuote (Call name_ args_ _) = qdef name_ $: (qvar <$> args_)
  toQuote (Assn expr _) = 'return $: [toQuote expr]
  toQuote (Guard a b) = 'guard $: [infixE (Just $ qvar a) (varE '(==)) (Just $ toQuote b)]
  toQuote (Sum exprs) = 'msum $: [listE $ NE.toList $ toQuote <$> exprs]
  toQuote (Bind stmts rets) = doE (NE.toList (go <$> stmts) ++ [noBindS quoteRets])
    where

      quoteRets = do
        let exprs = qvar <$> rets
        let e = case exprs of
              [e'] -> e'
              _ -> tupE exprs
        'return $: [e]

      go :: Lang -> Q Stmt
      go stmt = do
        let vs = pterm <$> outVars stmt
        (case vs of
          [] -> noBindS
          [v] -> bindS v
          _ -> bindS (tupP vs)
          ) (toQuote stmt)


instance Quotable ProgramTypes Dec where

  toQuote (ProgramTypes l) = do
    dataD
        (cxt [])
        typeName
        [] Nothing
        (go <$> l)
        [derivClause Nothing [conT ''Show, conT ''Eq]]
    where
      go :: (String, Int) -> Q Con
      go (n, i) = normalC (mkName n) (replicate i $
        bangType
            (bang noSourceUnpackedness noSourceStrictness)
            (conT typeName)
        )
      typeName = mkName "Term"

instance Quotable Program ProgramDec where
  toQuote (Program types_ defs_ goal_) = do
    t <- toQuote types_
    d <- concat <$> mapM toQuote defs_
    b <- traverse (varE . qdef) goal_
    return $ ProgramDec (t:d) b


























addoPrg = Program {
    types = ProgramTypes [("Zero", 0), ("Succ", 1)],
    goal = Nothing,
    defs = [
        Def {
            name = "addoIIO",
            args = [x, y],
            body = Sum $ NE.fromList [
                Bind (NE.fromList [
                    Guard x zro
                ]) [y],
                Bind (NE.fromList [
                    Assn (Var x) (suc x'),
                    Call "addoIIO" [x', y] [z'],
                    Assn (suc z') (Var z)
                ]) [z]
            ]
        },
        Def {
            name = "addoOOI",
            args = [z],
            body = Sum $ NE.fromList [
                Bind (NE.fromList [
                    Assn (Var z) (Var y),
                    Assn zro (Var x)
                ]) [x, y],
                Bind (NE.fromList [
                    Assn (Var z) (suc z'),
                    Call "addoOOI" [z'] [x', y],
                    Assn (suc x') (Var x)
                ]) [x, y]
            ]
        }
    ]
}
    where
        x = NormalVar 0
        y = NormalVar 1
        z = NormalVar 2
        x' = NormalVar 3
        z' = NormalVar 4

        zro = Con "Zero" []
        suc t = Con "Succ" [t]