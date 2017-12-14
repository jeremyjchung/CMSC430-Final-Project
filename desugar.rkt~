#lang racket

; by Jeremy Chung

(provide desugar)
(require "utils.rkt")


(define (improper-args? e)
  (match e
    ['() #f]
    [(cons a b) (improper-args? b)]
    [a #t]
    ))

(define (desugar-aux e)
  (match e
    
    [`(letrec* ([,xs ,es] ...) ,e1)
     (define init (map (lambda (x) `(,x 'undefined)) xs))
     (define bangs (map (lambda (x y) `(set! ,x ,y)) xs es))
     (desugar-aux `(let (,@init) (begin ,@bangs ,e1)))
     ]
    [`(letrec ([,xs ,es] ...) ,e1)
     (define init (map (lambda (x) `(,x 'undefined)) xs))
     (define ts (map (lambda (x) (gensym x)) xs))
     (define args->vals (map (lambda (t arg) `(,t ,arg)) ts es))
     (define bangs (map (lambda (x t) `(set! ,x ,t)) xs ts))
     (desugar-aux `(let (,@init) (let (,@args->vals) (begin ,@bangs ,e1))))
     ]

    
    [`(let* ([,xs ,es] ...) ,e1)
     (if (null? xs)
         (desugar-aux e1)
         (match e
           [`(let* (,x->v . ,vars->vals) ,_)
            (desugar-aux `(let (,x->v) (let* ,vars->vals ,e1)))
            ]))
     ]
    [`(let ([,xs ,es] ...) ,e1)
     (define args (foldl (lambda (x e acc) `(,@acc [,x ,(desugar-aux e)])) `() xs es))
     `(let (,@args) ,(desugar-aux e1))
     ]
    [`(let ,x ([,xs ,es] ...) ,e1)
     (desugar-aux `(letrec ([,x (lambda (,@xs) ,e1)]) (,x ,@es)))
     ]

    [`(lambda () ,e1)
     `(lambda () ,(desugar-aux e1))
     ]
    [`(lambda (,xs) ,e1)
     `(lambda (,xs) ,(desugar-aux e1))
     ]
    [`(lambda ,(? improper-args? xs) ,e1)
     (define flat (flatten xs))
     (match flat
       [`(,must ... ,op)
        (define num-req (length must))
        (define num -1)
        (define work (map (lambda (x)
                            (set! num (+ num 1))
                            `(list-tail x (quote ,num)))
                          must))
        (define please (map (lambda (m w) `[,m (car ,w)]) must work))
        `(lambda x ,(desugar-aux `(let (,@please [,op (list-tail x (quote ,num-req))]) ,e1)))
        ])    
     ]
    [`(lambda (,xs . ,rest) ,e1)
     `(lambda (,xs ,@rest) ,(desugar-aux e1))
     ]
    [`(lambda ,x ,e1)
     `(lambda ,x ,(desugar-aux e1))
     ]

    [`(call/cc ,e1)
     `(call/cc
       ,(desugar-aux
         `(lambda (k)
            (,e1 (let ([k-stack %wind-stack])
                   (lambda (x)
                     (begin (%do-wind k-stack)
                            (k x))))))))
     ]
    [`(dynamic-wind ,e1 ,e2 ,e3)
     `(%dynamic-wind ,(desugar-aux e1) ,(desugar-aux e2) ,(desugar-aux e3))
     ]
    [`(guard (,x ,clauses ...) ,body)
     (desugar-aux 
      `(let ([cc (call/cc (lambda (k) k))])
         (if (cons? cc)
             (let ([,x (car cc)]) 
                (cond ,@clauses [else (raise ,x)]))
             (dynamic-wind
              (lambda () (set! %exception-handler (cons cc %exception-handler)))
              (lambda () ,body)
              (lambda () (set! %exception-handler (cdr %exception-handler)))
              ))))
     ]
    [`(raise ,e1)
     (desugar-aux `((car %exception-handler) (cons ,e1 '())))
     ]

    [`(delay ,e1)
     (desugar-aux `(list 'promise-tag (lambda () ,e1) (make-vector '2)))
     ]
    [`(force ,e1)
     (desugar-aux `(let ([a (car (cdr ,e1))] [mv (car (cdr (cdr ,e1)))])
                     (if (eq? (vector-ref mv '0) 'forced)
                         (vector-ref mv '1)
                         (begin
                           (vector-set! mv '0 'forced)
                           (vector-set! mv '1 (a))
                           (vector-ref mv '1)
                           ))))
     ]

    [`(and)
     (desugar-aux `(quote #t))
     ]
    [`(and ,e1)
     (desugar-aux e1)
     ]
    [`(and ,e1 . ,es)
     (desugar-aux `(let ([x ,e1])
                  (if x (and ,@es) (quote #f))))
     ]  
    [`(or)
     (desugar-aux `(quote #f))
     ]
    [`(or ,e1 . ,es)
     (desugar-aux `(let ([x ,e1])
                     (if x x (or ,@es))))
     ]  

    [`(cond)
     (desugar-aux `(void))
     ]
    [`(cond ,es)
     (match es
       [`(else ,exp)
        (desugar-aux exp)
        ]
       [`(,b ,exp)
        (desugar-aux `(if ,b ,exp (void)))
        ]
       [`(,b)
        (desugar-aux `(let ([x ,b])
                        (if x x (void))))
        ])
     ]
    [`(cond . ,es)
     (define next (cdr es))
     (match (car es)
       [`(else ,exp)
        (desugar-aux exp)
        ]
       [`(,b ,exp)
        (desugar-aux `(if ,b ,exp (cond ,@next)))
        ]
       [`(,b)
        (desugar-aux `(let ([x ,b])
                        (if x x (cond ,@next))))
        ]
       )
     ]

    
    [`(case ,e1 . ,cond-clause)
     (if (null? cond-clause)
         (desugar-aux `(void))
         (match (car cond-clause)
           [`(else ,e2)
            (desugar-aux e2)
            ]
           [`(,lst ,val)
            (define lst0 (map (lambda (x) (desugar-aux `(quote ,x))) lst))
            (desugar-aux `(if (list? (member ,e1 (list ,@lst0)))
                 ,val
                 (case ,e1 ,@(cdr cond-clause))
                 ))
            ]
           ))
     ]
    [`(if ,e1 ,e2 ,e3)
     `(if ,(desugar-aux e1) ,(desugar-aux e2) ,(desugar-aux e3))
     ]
    [`(when ,e1 ,e2)
     (desugar-aux `(if ,e1 ,e2 void))
     ]
    [`(unless ,e1 ,e2)
     (desugar-aux `(if ,e1 void ,e2))
     ]

    
    [`(set! ,x ,e1)
     `(set! ,x ,(desugar-aux e1))
     ]

    [`(begin ,e1)
     (desugar-aux e1)
     ]
    [`(begin ,e1 . ,es)
     (desugar-aux `(let ([,(gensym '_) ,e1]) (begin ,@es)))
     ]

    [`(apply ,e1 ,e2)
     `(apply ,(desugar-aux e1) ,(desugar-aux e2))
     ]

    
    [`(promise? ,x)
     (desugar-aux `(if (and (not (null? ,x)) (list? ,x) (eq? (car ,x) 'promise-tag)) (quote #t) (quote #f)))
     ]
    [`(,(? prim? op) . ,args)
     (foldl (lambda (x acc) `(,@acc ,(desugar-aux x))) `(prim ,op) args)
     ]
    [`promise?
     (desugar-aux `(lambda (arg) (promise? arg)))
     ]
    [(? prim? op)
     `(lambda args (apply-prim ,op args))
     ]
    
    [(? symbol? x)
     x
     ]
    [`(quote ,(? datum? dat))
     `(quote ,dat)
     ]

    [`(,fun . ,args)
     (map (lambda (x) (desugar-aux x)) (cons fun args))
     ]
    

    ;...
    [else '()]))


(define (desugar e)
 
  (define (wrap-with-lib e)
    `(let* ([%wind-stack '()]
            [common-tail (lambda (x y)
                           (let ((lx (length x))
                                 (ly (length y)))
                             (let loop ([x (if (> lx ly) (drop x (- lx ly)) x)]
                                        [y (if (> ly lx) (drop y (- ly lx)) y)])
                               (if (eq? x y)
                                   x
                                   (loop (cdr x) (cdr y))))))]
            [%do-wind (lambda (new)
                        (unless (eq? new %wind-stack) 
                                (let ([tail (common-tail new %wind-stack)])
                                  (begin
                                    (let f ((l %wind-stack))
                                      (unless (eq? l tail)
                                              (begin
                                                (set! %wind-stack (cdr l))
                                                ((cdr (car l)))
                                                (f (cdr l)))))
                                    (let f ([l new])
                                      (unless (eq? l tail)
                                              (begin
                                                (f (cdr l))
                                                ((car (car l)))
                                                (set! %wind-stack l))))))))]
            [%dynamic-wind (lambda (pre body post)
                              (begin
                                (pre)
                                (set! %wind-stack (cons (cons pre post) %wind-stack))
                                (let ([v (body)])
                                  (begin
                                    (set! %wind-stack (cdr %wind-stack))
                                    (post)
                                    v))))]
            [%exception-handler '()])
           ,e))

  (desugar-aux (wrap-with-lib e))
  ;;(desugar-aux e)
   )





; I, Jeremy Chung, pledge on my honor that I have not given or 
; received any unauthorized assistance on this project.
