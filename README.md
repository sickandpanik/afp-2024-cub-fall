# HW02
## Soft deadline: 23:59 03.10.2024
## Hard deadline: 23:59 10.10.2024

1. (8 points) Implement the compiler and interpreter in recap/src/Imp/SLang.hs

   You have to compile expression with GADT-style verified types into expression with GADT-style verified types and variable bindings
   - Data `S` and `Z` represents Peano numbers
   - Data `GEQ` represents greater or equal relation on Peano numbers
   - Argument `iv` in `Var` constructor represents value addressing in a style of de Bruijn index
   - New parameter `i` in `SExpr` represents minimal required bindings in the context for this expression to be correct by construction

