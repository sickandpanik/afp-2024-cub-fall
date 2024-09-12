module RPNStack where

import Control.Monad.State
import Control.Monad (when, guard)
import Text.Read (readMaybe)
import Control.Applicative (Alternative(empty))

-- no pop 
-- too many stuff on the stack left 
-- invalid op -> pattern match and fail if smth goes wrong
-- invalid number -> readMaybe @Int


-- Evaluator for formulas written in Reverse Polish Notation 
type Stack = [Int]
type EvalM = StateT Stack Maybe
                        --  ^ inner monad

push :: Int -> EvalM ()
push x = modify (x:)

pop :: EvalM Int
pop = do
  (h:t) <- get
  put t
  return h

-- pop :: EvalM Int
-- pop = do
--   stack <- get
--   when (null stack) $ lift Nothing
--   put (tail stack)
--   return (head stack)

readSafe :: (Read a, Monad m, Alternative m) => String -> m a 
readSafe str = 
  case readMaybe str of 
    Just n -> return n 
    Nothing -> empty

-- singletonStack :: EvalM ()
-- singletonStack = do
--   l <- gets length
--   guard (l == 1)

singletonStack :: EvalM () 
singletonStack = do 
  state <- get 
  if length state == 1 
  then return () 
  else fail "Not a singleton stack"

eval :: String -> Maybe Int
eval expr =
    evalStateT go []
  where
    go = mapM_ step (words expr) >> singletonStack >> pop
    step "+" = binOp (+)
    step "-" = binOp (-)
    step "*" = binOp (*)
    step t = do 
      n <- readSafe t 
      push n 
    binOp op = do
      x <- pop
      y <- pop
      push (op y x)

main :: IO ()
main = do 
  print $ eval "1 2 +"         -- 3
  print $ eval "1 2 *"         -- 2
  print $ eval "1 2 -"         -- -1
  print $ eval "1 2 + 3 * 4 -" -- 5 
  print $ eval "" 
  print $ eval "2 a +"
  print $ eval "1 2 /"
  print $ eval "1 +"
  print $ eval "1 2"

-- -- ghci> eval "a 2 +"
-- -- *** Exception: Prelude.read: no parse
-- -- ghci> eval "2 +"
-- -- *** Exception: Prelude.head: empty list
-- -- ghci> eval ""
-- -- ghci> eval "- 1 2"
-- -- 2
-- -- ghci> eval "1 2 3 -"
-- -- -1
