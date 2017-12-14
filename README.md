# CMSC430-Final-Wahooo
Final project for CMSC430 (compilers)
Jeremy Chung - I did not cheat

This final project is a compilation of the previous projects we did this semester. The compiler.rkt file takes in a program and converts it in a series of steps to a final intermediate representation that can be compiled by Clang. 

## Part I

These are the following intermediate representations (IRs) in order ...

##### IR-0: (top-level prog)
  desugar define statements into letrec\*, quote datums, add implicit begins, and desugar quasiquote/unquote

##### IR-1: (simplify-ir (desugar IR-0))
  desugar an input language with forms such as letrec(\*), dynamic wind, guard/raise, first-class primitives, delay/force to a  smaller output langauge <br/>
  simplify the ir by adding some functions and simplifying how some of the primitives are used

##### IR-2: (cps-convert (anf-convert (alphatize (assignment-convert IR-1))))
  remove set! from the language by boxing all mutable variables <br/>
  alpha rename all variables so that they are unique (shadowing will no longer occur) <br/>
  convert to administrative normal form (lift all subexpressions to be bound in a let statement) <br/>
  convert to continuation passing style (remove call/cc and invoke the current continuation at return points) <br/>

##### IR-3: (proc->llvm (closure-convert IR-3))
  remove lambda abstractions and replace them with a closure object, lift atomic expressions (other than variable references) to be let bound <br/>
  transform into LLVM code that when combined with the header.ll file can be compiled with Clang 

## Part II
