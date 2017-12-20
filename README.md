# CMSC430 Final Project
*I, Jeremy Chung, pledge on my honor that I have not given or received any unauthorized assistance on this examination*

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

```racket
(null? v) -> boolean? 
(v: any/c)
```
Returns #t if v is the empty list, #f otherwise

```racket 
(car p) -> any/c 
(p: pair?) 
```
Returns the first element of the pair p

```racket 
(cdr p) -> any/c 
(p: pair?) 
```
Returns the second element of the pair p

```racket 
(cons a d) -> pair? 
(a: any/c, d: any/c) 
```
Returns a pair where a is the first element and d is the second element

```racket
(append lst ...) -> lst?
(lst: list?)
```
Returns a list that is the result of appending n lists together

```racket
(number? v) -> boolean? 
(v: any/c)
```
Returns #t if v is a number, #f otherwise

```racket
(+ v ...+) -> number? 
(v: variable (with numeric value) | subexpresssion (with numeric value) | (? number? n))
```
Returns the sum of one or more numbers

```racket
(- v ...+) -> number? 
(v: variable (with numeric value) | subexpresssion (with numeric value) | (? number? n))
```
If v represents (v0, v1, v2, ..., vn), returns (v0 - v1 - v2 - ... - vn)

```racket
(* v ...+) -> number? 
(v: variable (with numeric value) | subexpresssion (with numeric value) | (? number? n))
```
Returns the product of one or more numbers

```racket
(= v v) -> boolean? 
(v: variable (with numeric value) | subexpresssion (with numeric value) | (? number? n))
```
Returns #t if two numeric values are equal, #f otherwise

```racket
(<= v1 v2) -> boolean? 
(v1/v2: variable (with numeric value) | subexpresssion (with numeric value) | (? number? n))
```
Returns #t if v1 is less than or equal to v2, #f otherwise

```racket
(> v1 v2) -> boolean? 
(v1/v2: variable (with numeric value) | subexpresssion (with numeric value) | (? number? n))
```
Returns #t if v1 is greater than v2, #f otherwise

```racket
(not e) -> boolean? 
(e: variable (with boolean value) | subexpression (with boolean value) | (? boolean? b))
```
Returns #t if e evaluates to #f, #f otherwise

## Part II

### Fixed Runtime Errors

The following runtime errors each have their own testing folder (ex. tests/index-out-of-bounds). Each of these runtime errors will result in the program evaluating and returning an error message. The testing suite for these runtime errors checks whether the proper error message is returned for each corresponding test.

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
##### Uninitialized Variables
In the alphatize step of the cps pass, place a halt statement if a variable to be alphatized has never been referenced

```racket
[(? symbol? x)
  (if (hash-has-key? env x)                
    (hash-ref env x)
    `(prim halt '"run-time error: use of uninitialized variable"))]
```
##### Applying Non-Function Values
In the desugar pass, place a halt statement if the first argument in function position is not a procedure

```racket
[`(,fun . ,args)
  (define args+ (map (lambda (x) (desugar-aux x)) args))
  `(let ([f ,(desugar-aux fun)])
    (if (prim procedure? f)
      (f ,@args+)
      (prim halt '"run-time error: application of a non-procedure")))
]
```
##### Index Out Of Bounds Exception (for vectors)
In desugar pass, place a halt statement if an attempt to access unallocated memory is made (this also required tweaks to the header.cpp functions ... *prim_vector_45set_33, prim_vector_45ref, prim_vector_45length, prim_make_45vector*)

```racket
[`(vector-set! ,v ,i ,val)
     (define v+ (desugar-aux v))
     (define i+ (desugar-aux i))
     (define val+ (desugar-aux val))
     `(if (prim and (prim < ,i+ (prim vector-length ,v+)) (prim >= ,i+ '0))
          (prim vector-set! ,v+ ,i+ ,val+)
          (prim halt '"run-time error: index out of bounds exception"))
     ]
    [`(vector-ref ,v ,i)
     (define v+ (desugar-aux v))
     (define i+ (desugar-aux i))
     `(if (prim and (prim < ,i+ (prim vector-length ,v+)) (prim >= ,i+ '0))
          (prim vector-ref ,v+ ,i+)
          (prim halt '"run-time error: index out of bounds exception"))
     ]
```

##### Function is provided too few arguments
In the closure convert remove-varargs phase, check whether the number of paramters being passed to a function satisfies the required number of arguments

```racket
[`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
  ; turns (xs ...) into x and immediately into (x)
  ; by adding the needed car/cdr calls and let bindings
  (define gx (gensym 'rvp))
  (define gx+e
    (foldr (lambda (x gx+e)
      (define halt (gensym 'halt))
      (define str (gensym 'str))
      (define gx (gensym 'rvp))
      (define b (gensym 'b))
      (cons gx
        `(let ([,b (prim null? ,gx)])
          (if ,b
              (let ([,str '"run-time error: function is provided too few arguments"])
                (let ([,halt (prim halt ,str)])
                  (,halt ,halt)))
              (let ([,x (prim car ,gx)])
                (let ([,(car gx+e) (prim cdr ,gx)])
                  ,(cdr gx+e)))
              ))))
      (cons (gensym 'na) (remove-varargs body))
    xs))
  `(let ([,x (lambda (,(car gx+e)) ,(cdr gx+e))])
    ,(remove-varargs e0))]
```

##### Other Runtime Errors

*Function with too many values in argument position* <br/>
*Memory limit exceeded (stackoverflow)* <br/>
*Infinite loop*

## Part III

I implemented a hash-map(hamt implementation) with the following supported operations

```racket
(hash)
```
Returns an immutable hash-map object

```racket
(hash-ref h k)
(h: hash?, k: any/c)
```
Returns the object associated with the given key k in the hashmap h (assumes that k is in h)

```racket
(hash-set h k v)
(h: hash?, k: any/c, v:any/c)
```
Returns an immutable hashmap object that is the result of adding the key-value pair into hashmap h

```racket
(hash-remove h k)
(h: hash?, k: any/c)
```
Returns an immutable hashmap object that is the result of removing k from hashmap h<br/><br/>

```c++
class key
{
public:
    const u64 x;

    key(u64 x)
        : x(x)
    {}

    u64 hash() const
    {
        const u8* data = reinterpret_cast<const u8*>(this);
        u64 h = 0xcbf29ce484222325;
        for (u32 i = 0; i < sizeof(key); ++i && ++data)
        {
            h = h ^ *data;
            h = h * 0x100000001b3;
        }

        return h;
    }

    bool operator==(const key& t) const
    {
        return t.x == this->x;
    }
};

class value
{
public:
  const u64 v;

  value(u64 v)
      : v(v)
  {}
};

u64 prim_hash()
{
  const hamt<key, value>* h = new ((hamt<key,value>*)malloc(sizeof(hamt<key,value>))) hamt<key,value>();
  return ENCODE_OTHER(h);
}

u64 prim_hash_45ref(u64 h, u64 k)
{
  ASSERT_TAG(h, OTHER_TAG, "first argument to hash-ref must be a hash")

  const hamt<key,value>* hmap = (hamt<key, value>*)DECODE_OTHER(h);
  const key* const t = new ((key*)malloc(sizeof(key))) key(k);
  const value* const v = hmap->get(t);

  return v->v;
}

u64 prim_hash_45set(u64 h, u64 k, u64 v)
{
  ASSERT_TAG(h, OTHER_TAG, "first argument to hash-set must be a hash")

  const hamt<key,value>* hmap = (hamt<key, value>*)DECODE_OTHER(h);
  const key* const tk = new ((key*)malloc(sizeof(key))) key(k);
  const value* const tv = new ((value*)malloc(sizeof(value))) value(v);
  return ENCODE_OTHER(hmap->insert(tk,tv));
}

u64 prim_hash_45remove(u64 h, u64 k)
{
  ASSERT_TAG(h, OTHER_TAG, "first argument to hash-remove must be a hash")

  const hamt<key,value>* hmap = (hamt<key, value>*)DECODE_OTHER(h);
  const key* const t = new ((key*)malloc(sizeof(key))) key(k);
  return ENCODE_OTHER(hmap->remove(t));
}

u64 prim_hash_45_has45_key64(u64 h, u64 k)
{
  ASSERT_TAG(h, OTHER_TAG, "first argument to hash-has-key? must be a hash")

  const hamt<key,value>* hmap = (hamt<key, value>*)DECODE_OTHER(h);
  const key* const t = new ((key*)malloc(sizeof(key))) key(k);
  const value* const v = hmap->get(t);

  if (v == 0) {
    return V_FALSE;
  }

  return V_TRUE;
}
```
Code in header.cpp that was written to implement this feature

## Part IV

Not implemented ...

## Example Test Run

![Alt text](./run-test.png?raw=true "Example Test Run")

## Works Cited

Thomas Gilray, Kristopher Micinski - hamt.h, compat.h 
