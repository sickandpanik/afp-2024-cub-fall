# HW01
## Soft deadline: 23:59 03.10.2024
## Hard deadline: 23:59 10.10.2024

1. (8 points) Implement the compiler and interpreter in recap/src/Imp/SLang.hs

   You have to compile expression with GADT-style verified types into expression with GADT-style verified types and variable bindings
   - Data `S` and `Z` represents Peano numbers
   - Data `GEQ` represents greater or equal relation on Peano numbers
   - Argument `iv` in `Var` constructor represents value addressing in a style of de Bruijn index
   - New parameter `i` in `SExpr` represents minimal required bindings in the context for this expression to be correct by construction

## Notes 

* Make a fork of this repository and checkout the branch `HW01`.
* Write your code in the [recap/src/Imp/SLang.hs](recap/src/Imp/Eval.hs) file: replace `undefined` with your definitions.
* There are some small programs defined in [recap/src/Imp/Exprs.hs](recap/src/Imp/Programs.hs) which are evaluated in [recap/app/Main.hs](recap/app/Main.hs) -- make sure that your interpreter works correctly. 
* It would be lovely if you add more programs to test the behavior of you interpreter. 
* When finished, open a pull request into the main repo. Make sure to *put your name* in the title of the PR.  
