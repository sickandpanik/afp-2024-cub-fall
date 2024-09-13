module Main (main) where

import Imp.Programs
import Imp.Eval

main = do
  mapM_ runProgram programs