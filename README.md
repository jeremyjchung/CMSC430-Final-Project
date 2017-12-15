# CMSC430-Final-Wahooo
Final project for CMSC430 (compilers)
Jeremy Chung - I did not cheat

This final project is a compilation of the previous projects we did this semester. The compiler.rkt file takes in a program and converts it in a series of steps to a final intermediate representation that can be compiled by Clang. 

## Part I

### Intermediate Representations

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
  
### Supported Primitive Operations

###### (null? v) -> boolean?
Returns #t if v is the empty list, #f otherwise

###### (car p) -> any/c (p: pair?)
Returns the first element of the pair p

###### (cdr p) -> any/c (p: pair?)
Returns the second element of the pair p

###### (cons a d) -> pair? (a: any/c, d: any/c)
Returns a pair where a is the first element and d is the second element

###### (number? v) -> boolean? (v: any/c)
Returns #t if v is a number, #f otherwise

###### (+ v ...+) -> number? (v: variable (with numeric value) | subexpresssion (with numeric value) | (? number? n))
Returns the sum of one or more numbers

###### (- v ...+) -> number? (v: variable (with numeric value) | subexpresssion (with numeric value) | (? number? n))
If v represents (v0, v1, v2, ..., vn), returns (v0 - v1 - v2 - ... - vn)

###### (* v ...+) -> number? (v: variable (with numeric value) | subexpresssion (with numeric value) | (? number? n))
Returns the product of one or more numbers

###### (= v v) -> boolean? (v: variable (with numeric value) | subexpresssion (with numeric value) | (? number? n))
Returns #t if two numeric values are equal, #f otherwise

###### (<= v1 v2) -> boolean? (v1/v2: variable (with numeric value) | subexpresssion (with numeric value) | (? number? n))
Returns #t if v1 is less than or equal to v2, #f otherwise

###### (> v1 v2) -> boolean? (v1/v2: variable (with numeric value) | subexpresssion (with numeric value) | (? number? n))
Returns #t if v1 is greater than v2, #f otherwise

###### (not e) -> boolean? (e: variable (with boolean value) | subexpression (with boolean value) | (? boolean? b))
Returns #t if e evaluates to #f, #f otherwise

## Part II

### Fixed Runtime Errors

##### Division by Zero
In top-level pass, insert an if statement into the output desugar for the following two cases

```racket
[`(/ ,arg)
 `(if (= ,(main-aux arg) '0)
    (halt '"run-time error: division by zero")
    `(/ ,(main-aux arg)))
]
[`(/ . ,args)
  (define args+ (map (lambda (x) (main-aux x)) args))
  `(if (= (* ,@(cdr args+)) '0)
    (halt '"run-time error: division by zero")
    (/ ,@args+))
]
```


