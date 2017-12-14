#lang racket

(provide top-level)

(require "utils.rkt")

; By Jeremy Chung

(define (top-level e)
  (define (improper-args? e)
    (match e
      ['() #f]
      [(cons a b) (improper-args? b)]
      [a #t]
      ))

  (define (opt-args? e)
    (match e
      [(cons a b)
       (match a
         [`(,x ,v) #t]
         [else (opt-args? b)])
       ]
      [else #f]))

  (define (opt-args-aux lst)
    (foldr (lambda (x acc)
             (match x
               [`(,a ,b) (cons (car acc) (cons x (cdr acc)))]
               [else (cons (cons x (car acc)) (cdr acc))])) (cons '() '()) lst))

  (define qq-level 0)
  (define (main-aux e)
    (match e
      [`(letrec* ([,xs ,vs] ...) ,es ...)
       (define mappings (map (lambda (x v) `(,(main-aux x) ,(main-aux v))) xs vs))
       `(letrec* ,mappings ,(main-aux `(begin ,@es)))
       ]
      [`(letrec ([,xs ,vs] ...) ,es ...)
       (define mappings (map (lambda (x v) `(,(main-aux x) ,(main-aux v))) xs vs))
       `(letrec ,mappings ,(main-aux `(begin ,@es)))
       ]
      [`(let* ([,xs ,vs] ...) ,es ...)
       (define mappings (map (lambda (x v) `(,(main-aux x) ,(main-aux v))) xs vs))
       `(let* ,mappings ,(main-aux `(begin ,@es)))
       ]
      [`(let ([,xs ,vs] ...) ,es ...)
       (define mappings (map (lambda (x v) `(,(main-aux x) ,(main-aux v))) xs vs))
       `(let ,mappings ,(main-aux `(begin ,@es)))
       ]
      [`(let ,f ([,xs ,vs] ...) ,es ...)
       (define mappings (map (lambda (x v) `(,(main-aux x) ,(main-aux v))) xs vs))
       `(let ,f ,mappings ,(main-aux `(begin ,@es)))
       ]
      [`(lambda ,(? opt-args? all) ,es ...)
       (define helper (opt-args-aux all))
       (define xs (car helper))
       (define ds->vs (cdr helper))
       (define ds (map (lambda (x) (match x [`(,a ,b) a])) ds->vs))
       (define vs (map (lambda (x) (match x [`(,a ,b) b])) ds->vs))
       
       (define args (gensym 'opt))
       (define vals (gensym 'opt-vals))
       (define ind (gensym 'index))
       (define i -1)
       (define pop-vect (map (lambda (x) (set! i (+ i 1))
                               `(define ,(gensym 'a)
                                  (if (eq? "nil" (vector-ref ,vals ,i)) (vector-set! ,vals ,i ,x) (void)))
                               ) vs))
       (set! i -1)
       (define ds->vect (map (lambda (x) (set! i (+ i 1)) `(define ,x (vector-ref ,vals ,i))) ds))
       (main-aux `(lambda (,@xs . ,args)
                    (define ,ind 0)
                    (define ,vals (make-vector ,(length ds) "nil"))
                    (define ,(gensym 'a) (map (lambda (x) (set! ,ind (+ ,ind 1)) (vector-set! ,vals (- ,ind 1) x)) ,args))
                    ,@pop-vect
                    ,@ds->vect
                    ,@es))
       ]
          
      [`(lambda ,x ,es ...)
       `(lambda ,x ,(main-aux `(begin ,@es)))
       ]
      [`(dynamic-wind ,e0 ,e1 ,e2)
       `(dynamic ,(main-aux e0) ,(main-aux e1) ,(main-aux e2))
       ]
      [`(guard (,x ,cond-clause ...) ,es ...)
       (define cond-clause+
         (map (lambda (x)
                (match x
                  [`(else ,es ...)
                   `(else ,(main-aux `(begin ,@es)))
                   ]
                  [`(,cond)
                   `(,(main-aux cond))
                   ]
                  [`(,cond ,es ...)
                   `(,(main-aux cond) ,(main-aux `(begin ,@es)))
                   ])) cond-clause))
       `(guard (,(main-aux x) ,@cond-clause+) ,(main-aux `(begin ,@es)))
       ]
      [`(raise ,e)
       `(raise ,(main-aux e))
       ]
      [`(delay ,e)
       `(delay ,(main-aux e))
       ]
      [`(force ,e)
       `(force ,(main-aux e))
       ]
      [`(and ,es ...)
       `(and ,@(map (lambda (x) (main-aux x)) es))
       ]
      [`(or ,es ...)
       `(or ,@(map (lambda (x) (main-aux x)) es))
       ]
      [`(cond ,cond-clause ...)
       (define cond-clause+
         (map (lambda (x)
                (match x
                  [`(else ,es ...)
                   `(else ,(main-aux `(begin ,@es)))
                   ]
                  [`(,cond)
                   `(,(main-aux cond))
                   ]
                  [`(,cond ,es ...)
                   ;(pretty-print cond)
                   `(,(main-aux cond) ,(main-aux `(begin ,@es)))
                   ])) cond-clause))
       `(cond ,@cond-clause+)
       ]
      [`(case ,e ,case-clause ...)
       (define case-clauses+
         (map (lambda (x)
                (match x
                  [`(else ,es ...)
                   `(else ,(main-aux `(begin ,@es)))
                   ]
                  [`(,dats ,es ...)
                   `(,dats ,(main-aux `(begin ,@es)))
                   ])) case-clause))
       `(case ,(main-aux e) ,@case-clauses+)
       ]
      [`(if ,e0 ,e1 ,e2)
       `(if ,(main-aux e0) ,(main-aux e1) ,(main-aux e2))
       ]
      [`(when ,e ,es ...)
       `(when ,(main-aux e) ,(main-aux `(begin ,@es)))
       ]
      [`(unless ,e ,es ...)
       `(unless ,(main-aux e) ,(main-aux `(begin ,@es)))
       ]
      [`(set! ,x ,e)
       `(set! ,x ,(main-aux e))
       ]
      [`(begin ,es ...)
       (define xv.es (begin-aux es))
       (if (null? (car xv.es))
           `(begin (void) ,@(map (lambda (x) (main-aux x)) es))
           (main-aux `(letrec* ,(car xv.es) ,@(cdr xv.es))))
       ]
      [`(call/cc ,e)
       `(call/cc ,(main-aux e))
       ]
      [`(apply ,e0 ,e1)
       `(apply ,(main-aux e0) ,(main-aux e1))
       ]
      [(? symbol? x)
       x
       ]
      [(? prim? op)
       op
       ]
      [`(quasiquote ,qq)
       (set! qq-level (+ qq-level 1))
       (define a (qq-aux qq))
       (set! qq-level (- qq-level 1))
       a
       ]
      [`(quote ,(? datum? dat))
       `(quote ,dat)
       ]
      [(list 'unquote exp)
       ;(main-aux exp)
       (if (= qq-level 0)
           `(quote ,exp)
           (begin
             (set! qq-level (- qq-level 1))
             (let ([a (main-aux exp)])
               (set! qq-level (+ qq-level 1))
               a)))
       ]
      [(? natural? nat)
       `(quote ,nat)
       ]
      [(? string? str)
       `(quote ,str)
       ]
      [(? boolean? b)
       `(quote ,b)
       ]
      [`(,e . ,es)
       (map (lambda (x) (top-level x)) (cons e es))
       ]))

  (define (begin-aux es)
    (define xv.es
      (foldl (lambda (exp acc)
               (match exp
                 [`(define (,x ,xs ...) ,es0 ...)
                  (define lam `(lambda ,xs ,@es0))
                  (define x->v `(,x ,lam))
                  (cons (append (car acc) (list x->v)) (cdr acc))
                  ]
                 [`(define ,(? symbol? x) ,e)
                  (define x->v `(,x ,e))
                  (cons (append (car acc) (list x->v)) (cdr acc))
                  ]
                 [`(define ,(? improper-args? x) ,es0 ...)
                  (define lam `(lambda ,(cdr x) ,@es0))
                  (define x->v `(,(car x) ,lam))
                  (cons (append (car acc) (list x->v)) (cdr acc))
                  ]
                 [`(begin ,es ...)
                  (define aux (begin-aux es))
                  (cons (append (car acc) (car aux)) (append (cdr acc) (cdr aux)))
                  ]
                 [`(set! ,x ,e)
                  (define work (gensym 'pls))
                  (define x->v `(,work ,exp))
                  (cons (append (car acc) (list x->v)) (cdr acc))
                  ]
                 [else
                  (cons (car acc) (append (cdr acc) (list exp)))
                  ])) (cons '() '()) es))
    xv.es
    )
  
  (define (qq-aux qq)
    (match qq
      ['() `'()]
      [(cons a b)
       (match a
         [`unquote (car b)]
         [else
          (if (symbol? a)
              `(cons ',a ,(qq-aux b))
              `(cons ,(main-aux a) ,(qq-aux b)))
          ])
       ]
      [else
       (if (symbol? qq)
           `(quote ,qq)
           (main-aux qq))
       ]
      ))

   #;(define (pat-aux pat)
    (match pat
      [`(? ,e ,pat)
       `(? ,(top-level e) ,(pat-aux pat))
       ]
      [`(cons ,pat0 ,pat1)
       `(cons ,(pat-aux pat0) ,(pat-aux pat1))
       ]
      [else
       (top-level pat)
       ]
    ))
 
(main-aux e)
)



; I, Jeremy Chung, pledge on my honor that I have not given or received any
;unauthorized assistance on this assignment.

