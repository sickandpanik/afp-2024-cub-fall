{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Imp.TLang where

import Control.Monad.State
  ( MonadState (get),
    MonadTrans (lift),
    StateT,
    modify,
  )
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.Map.Strict as M
import qualified Imp.Lang as L
import Text.Read (readMaybe)

-- An expression of the imperative language Imp
data TExpr a where
  Var :: String -> TExpr Int
  Const :: Int -> TExpr Int
  BinOp :: L.Op -> TExpr Int -> TExpr Int -> TExpr Int
  Read :: TExpr Int
  Write :: TExpr Int -> TExpr ()
  Seq :: TExpr () -> TExpr a -> TExpr a
  If :: TExpr Int -> TExpr a -> TExpr a -> TExpr a
  LetIn :: String -> TExpr Int -> TExpr a -> TExpr a
  Skip :: TExpr ()

-- Possible errors reported during evaluation
data TError = UndefinedVar String | ParsingErr String
  deriving (Eq, Show)

-- Evaluation monad
type TEvalM = StateT L.VarMap (ExceptT TError IO)

evalExpr :: TExpr a -> TEvalM a
evalExpr (Var s) = do
  varMap <- get
  case M.lookup s varMap of
    Just x -> return x
    Nothing -> lift $ throwE $ UndefinedVar s
evalExpr (Const x) = return x
evalExpr (BinOp op e1 e2) = do
  x <- evalExpr e1
  y <- evalExpr e2
  case op of
    L.Plus -> return $ x + y
    L.Minus -> return $ x - y
evalExpr Read = do
  input <- lift $ lift getLine
  case readMaybe input of
    Just x -> return x
    Nothing -> lift $ throwE $ ParsingErr input
evalExpr (Write e) = do
  x <- evalExpr e
  lift $ lift $ print x
evalExpr (Seq c1 c2) = evalExpr c1 >> evalExpr c2
evalExpr (If e c1 c2) = do
  x <- evalExpr e
  if x == 0 then evalExpr c1 else evalExpr c2
evalExpr (LetIn s e1 e2) = do
  x <- evalExpr e1
  modify $ M.insert s x
  evalExpr e2
evalExpr Skip = return ()

data AnyTExpr where
  IntExpr :: TExpr Int -> AnyTExpr
  UnitExpr :: TExpr () -> AnyTExpr

data TCompileError = TypeError
  deriving (Eq, Show)

unpackPack :: (forall a. TExpr a -> TExpr a) -> AnyTExpr -> AnyTExpr
unpackPack f (IntExpr e) = IntExpr $ f e
unpackPack f (UnitExpr e) = UnitExpr $ f e

compile :: (Monad m) => L.Expr -> ExceptT TCompileError m AnyTExpr
compile (L.Var s) = return $ IntExpr $ Var s
compile (L.Const x) = return $ IntExpr $ Const x
compile (L.BinOp op e1 e2) = do
  e1' <- compile e1
  e2' <- compile e2
  case (e1', e2') of
    (IntExpr e1'', IntExpr e2'') -> return $ IntExpr $ BinOp op e1'' e2''
    _ -> throwE TypeError
compile L.Read = return $ IntExpr Read
compile (L.Write e) = do
  e' <- compile e
  case e' of
    IntExpr e'' -> return $ UnitExpr $ Write e''
    _ -> throwE TypeError
compile (L.Seq e1 e2) = do
  e1' <- compile e1
  e2' <- compile e2
  case e1' of
    UnitExpr e1'' -> return $ unpackPack (Seq e1'') e2'
    _ -> throwE TypeError
compile (L.If e c1 c2) = do
  e' <- compile e
  c1' <- compile c1
  c2' <- compile c2
  case (e', c1', c2') of
    (IntExpr e'', UnitExpr c1'', UnitExpr c2'') -> return $ UnitExpr $ If e'' c1'' c2''
    (IntExpr e'', IntExpr c1'', IntExpr c2'') -> return $ IntExpr $ If e'' c1'' c2''
    _ -> throwE TypeError
compile (L.LetIn s e1 e2) = do
  e1' <- compile e1
  e2' <- compile e2
  case e1' of
    (IntExpr e1'') -> return $ unpackPack (LetIn s e1'') e2'
    _ -> throwE TypeError
compile L.Skip = return $ UnitExpr Skip
