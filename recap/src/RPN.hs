module RPN where

import Control.Monad.State

-- Evaluator for formulas written in Reverse Polish Notation
type Stack = [Int]

type EvalM = State Stack

push :: Int -> EvalM ()
push x = undefined 

pop :: EvalM Int
pop = undefined 

eval :: String -> Int
eval expr =
    evalState go []
  where
    go = undefined 

main :: IO ()
main = do
  print $ eval "1 2 +" -- 3
  print $ eval "1 2 *" -- 2
  print $ eval "1 2 -" -- -1
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
