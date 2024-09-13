module RPN ( eval ) where

import Control.Monad.State

-- Evaluator for formulas written in Reverse Polish Notation 
type Stack = [Int]
type EvalM = State Stack

push :: Int -> EvalM ()
push x = do
  oldStack <- get
  put (x : oldStack)

pop :: EvalM Int
pop = do
  oldStack <- get 
  put (tail oldStack)
  return (head oldStack)

eval :: String -> Int
eval expr =
    evalState (go (words expr)) []
  where
    go :: [String] -> EvalM Int
    go [] = do
      pop
    go (h : t) = case h of
      h 
        | h `elem` ["+", "-", "*"] -> do
          a <- pop
          b <- pop
          case h of 
            "+" -> push (a + b)
            "-" -> push (b - a)
            "*" -> push (a * b)
          go t
        | otherwise -> do
          push (read h :: Int)
          go t

main :: IO ()
main = do 
  print $ eval "1 2 +"         -- 3
  print $ eval "1 2 *"         -- 2
  print $ eval "1 2 -"         -- -1
  print $ eval "1 2 + 3 * 4 -" -- 5 

-- ghci> eval "a 2 +"
-- *** Exception: Prelude.read: no parse
-- ghci> eval "2 +"
-- *** Exception: Prelude.head: empty list
-- ghci> eval ""
-- ghci> eval "- 1 2"
-- 2
-- ghci> eval "1 2 3 -"
-- -1
