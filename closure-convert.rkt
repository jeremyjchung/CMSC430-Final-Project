#lang racket

(require "cps.rkt")
(require "utils.rkt")
(require "desugar.rkt")

(provide closure-convert
         proc->llvm)

; Pass that removes lambdas and datums as atomic and forces them to be let-bound
;   ...also performs a few small optimizations
(define (simplify-ae e)
  (define (improper-args? e)
    (match e
      ['() #f]
      [(cons a b) (improper-args? b)]
      [a #t]))
  (define (args-req e n)
    (match e
      ['() n]
      [(cons a b) (args-req b (+ n 1))]
      [a n]))
  
  (define (wrap-aes aes wrap)
    (match-define (cons xs wrap+)
                  (foldr (lambda (ae xs+wrap)
                           (define gx (gensym 'arg))
                           (if (symbol? ae)
                               (cons (cons ae (car xs+wrap))
                                     (cdr xs+wrap))
                               (cons (cons gx (car xs+wrap))
                                     (lambda (e)
                                       (match ae
                                              [`(lambda ,xs ,body) 
                                               `(let ([,gx (lambda ,xs ,(simplify-ae body))])
                                                  ,((cdr xs+wrap) e))]
                                              [`',dat
                                               `(let ([,gx ',dat])
                                                  ,((cdr xs+wrap) e))])))))
                         (cons '() wrap)
                         aes))
    (wrap+ xs))
  (match e
         [`(let ([,x (lambda ,xs ,elam)]) ,e0)
          (define req-args (args-req xs 0))
          (define opt-args (improper-args? xs))
          `(let ([,x (lambda ,xs ,(simplify-ae elam))]) ,(simplify-ae e0))]

         [`(let ([,x ',dat]) ,e0)
          `(let ([,x ',dat]) ,(simplify-ae e0))]

         [`(let ([,x (prim ,op ,aes ...)]) ,e0)
          (wrap-aes aes (lambda (xs) `(let ([,x (prim ,op ,@xs)]) ,(simplify-ae e0))))]
         [`(let ([,x (apply-prim ,op ,aes ...)]) ,e0)
          (wrap-aes aes (lambda (xs) `(let ([,x (apply-prim ,op ,@xs)]) ,(simplify-ae e0))))]

         [`(if (lambda . ,_) ,et ,ef)
          (simplify-ae et)]
         [`(if '#f ,et ,ef)
          (simplify-ae ef)]
         [`(if ',dat ,et ,ef)
          (simplify-ae et)]
         [`(if ,(? symbol? x) ,et ,ef)
          `(if ,x ,(simplify-ae et) ,(simplify-ae ef))]

         [`(apply ,ae0 ,ae1)    ; case to be implemented
          (wrap-aes (list ae0 ae1) (lambda (xs) `(apply ,@xs)))]
         
         [`(,aes ...)
          (wrap-aes aes (lambda (xs) xs))
          ]))


; Helper to remove vararg lambdas/callsites
(define (remove-varargs e)
  (match e
         [`(let ([,x ',dat]) ,e0)
          `(let ([,x ',dat]) ,(remove-varargs e0))]
         [`(let ([,x (prim ,op ,xs ...)]) ,e0)
          `(let ([,x (prim ,op ,@xs)]) ,(remove-varargs e0))]
         [`(let ([,x (apply-prim ,op ,y)]) ,e0)
          `(let ([,x (apply-prim ,op ,y)]) ,(remove-varargs e0))]
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
         [`(let ([,x (lambda ,y ,body)]) ,e0)
          `(let ([,x (lambda (,y) ,(remove-varargs body))])
             ,(remove-varargs e0))]
         [`(if ,x ,e0 ,e1)
          `(if ,x ,(remove-varargs e0) ,(remove-varargs e1))]
         [`(apply ,f ,args)
          `(,f ,args)] 
         [`(,f ,xs ...) ; case not written
          (define empty (gensym 'empty))
          (define lst1 (map (lambda (x) (gensym 'args)) xs))
          (define (aux lst lst1 prev)
            (if (empty? lst)
                `(,f ,prev)
                (begin
                  `(let ([,(car lst1) (prim cons ,(car lst) ,prev)])
                     ,(aux (cdr lst) (cdr lst1) (car lst1))))))
          `(let ([,empty '()]) ,(aux (reverse xs) lst1 empty))
          ]
          
    ))


; call simplify-ae on input to closure convert, then remove vararg callsites/lambdas
(define (closure-convert cps)
  (define scps (simplify-ae cps))
  (define no-varargs-cps (remove-varargs scps))
  
  ; case not written; see our livecoding from class
  ; Exp x List[Proc] -> Exp x Set[Var] x List[Proc]
  (define (bottom-up e procs)
    (match e
      [`(let ([,x (apply-prim ,op ,v)]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
                     (bottom-up e0 procs))
       `((let ([,x (apply-prim ,op ,v)]) ,e0+)
         ,(set-remove (set-union free+ (list->set (list v))) x)
         ,procs+)
       ]
      [`(let ([,x (prim ,op ,xs ...)]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
                     (bottom-up e0 procs))
       `((let ([,x (prim ,op ,@xs)]) ,e0+)
         ,(set-remove (set-union free+ (list->set xs)) x)
         ,procs+)
       ]
      [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
       (match-define `(,e0+ ,free0+ ,procs0+)
                     (bottom-up e0 procs))
       (match-define `(,body+ ,freelam+ ,procs1+)
                     (bottom-up body procs0+))
       (define env-vars (foldl (lambda (x fr) (set-remove fr x))
                               freelam+
                               xs))
       (define ordered-env-vars (set->list env-vars))
       (define lamx (gensym 'lam))
       (define envx (gensym 'env))
       (define body++ (cdr (foldl (lambda (x count+body)
                                    (match-define (cons cnt bdy) count+body)
                                     (cons (+ 1 cnt)
                                           `(let ([,x (env-ref ,envx ,cnt)])
                                              ,bdy)))
                                  (cons 1 body+)
                                  ordered-env-vars)))
       `((let ([,x (make-closure ,lamx ,@ordered-env-vars)]) ,e0+)
         ,(set-remove (set-union free0+ env-vars) x)
         ((proc (,lamx ,envx ,@xs) ,body++) . ,procs1+))
       ]
      [`(let ([,x ',dat]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
                     (bottom-up e0 procs))
       `((let ([,x ',dat]) ,e0+)
         ,(set-remove free+ x)
         ,procs+)
       ]      
      [`(if ,(? symbol? x) ,e0 ,e1)
       (match-define `(,e0+ ,free0+ ,procs0+)
                     (bottom-up e0 procs))
       (match-define `(,e1+ ,free1+ ,procs1+)
                     (bottom-up e1 procs0+))
       `((if ,x ,e0+ ,e1+)
         ,(set-union free1+ free0+ (set x))
         ,procs1+)
       ]
      [`(,(? symbol? xs) ...)
       `((clo-app ,@xs)
         ,(list->set xs)
         ,procs)
       ]))
  
  (match-define `(,main-body ,free ,procs) (bottom-up no-varargs-cps '()))
  `((proc (main) ,main-body) . ,procs))



; Walk procedures and emit llvm code as a string
; (string-append "  %r0 = opcode i64 %a, %b \n"
;                "  %r1 = ... \n")
(define (proc->llvm proc)
  (define procs->args (hash))
  (define const-lst '())
  (define (llvm-lst proc env)
    (match proc
      [`(proc (,x . ,xs) ,e0)
       ;(set! func-lst (hash-set func-lst x name))
       (define params (map (lambda (x) (set! env (hash-set env x (~a "%" x))) (~a "i64 %" x)) xs))
       (define env1 (~a (gensym '%envptr)))
       (set! procs->args (hash-set procs->args (~a "@" x) xs))

       (if (string=? (~a x) "main")
           (~a "define void @proc_" x "(" (string-join params ",") ") {\n" (llvm-lst e0 env) "}\n\n")
           (~a "define void @" x "(" (string-join params ",") ") {\n"
               env1 " = inttoptr i64 " (hash-ref env (car xs)) " to i64*\n"
               (llvm-lst e0 (hash-set env (car xs) env1))
                         "}\n\n")
           )
       ]
      [`(let ([,x (apply-prim ,op ,arg)]) ,e0)
       (define reg (~a "%" (c-name x)))
       (~a reg " = call i64 @" (prim-applyname op) "(i64 " (hash-ref env arg) ")\n"
           (llvm-lst e0 (hash-set env x reg)))
       ]
      [`(let ([,x (prim ,op ,args ...)]) ,e0)
       (define reg (~a "%" (c-name x)))
       (define llvm-args (map (lambda (x) (~a "i64 " (hash-ref env x))) args))
       (~a reg " = call i64 @" (prim-name op) "(" (string-join llvm-args ",") ")\n"
           (llvm-lst e0 (hash-set env x reg)))
       ]
      [`(let ([,x (make-closure ,f . ,xs)]) ,e0)
       (define reg (~a "%" (c-name x)))
       (define clo (~a "%" (gensym 'cloptr)))
       (define fi (~a "%" (gensym 'f)))
       (define cnt 0)
       (define clo-env (map (lambda (x)
                              (define eptr (~a "%" (gensym 'eptr)))
                              (set! cnt (+ cnt 1))
                              (~a eptr " = getelementptr inbounds i64, i64* " clo ", i64 " cnt "\n"
                                  "store i64 " (hash-ref env x) ", i64* " eptr "\n"
                                  )) xs))
       (define eptr (~a "%" (gensym 'eptr)))
       (define void-args (map (lambda (x) "i64") (hash-ref procs->args (~a "@" f))))

       (~a clo " = call i64* @alloc(i64 " (/ (* 64 (length (cons f xs))) 8) ")\n"
           (string-join clo-env "")
           eptr " = getelementptr inbounds i64, i64* " clo ", i64 0\n"
           fi " = ptrtoint void(" (string-join void-args ",") ")* @" f " to i64\n"   
           "store i64 " fi ", i64* " eptr "\n"
           reg " = ptrtoint i64* " clo " to i64\n"
           (llvm-lst e0 (hash-set env x reg))
           )
       ]
      [`(let ([,x (env-ref ,envx ,nat)]) ,e0)
        (define reg (~a "%" (c-name x)))
        (define env1 (~a (gensym '%envptr)))
        (~a env1 " = getelementptr inbounds i64, i64* " (hash-ref env envx) ", i64 " nat "\n"
            reg " = load i64, i64* " env1 ", align 8\n"
            (llvm-lst e0 (hash-set env x reg)))
       ]
      [`(let ([,x (quote ,dat)]) ,e0)
       (define reg (~a "%" (c-name x)))
       (match dat
         [(? empty?)
          (~a reg " = call i64 @const_init_null()\n"
              (llvm-lst e0 (hash-set env x reg)))
          ]
         [(? void?)
          (~a reg " = call i64 @const_init_void()\n"
              (llvm-lst e0 (hash-set env x reg)))
          ]
         [(? string?)
          (define str (~a (gensym '@.str.)))
          (define length (+ (string-length dat) 1))
          (define glob (~a str " = global [" length " x i8] c\"" dat "\\00\", align 8"))
          (set! const-lst (cons glob const-lst))
          (~a reg " = call i64 @const_init_string(i8* getelementptr inbounds ([" length " x i8], [" length " x i8]* " str ", i32 0, i32 0))\n"
              (llvm-lst e0 (hash-set env x reg)))
          ]
         [(? integer?)
          (~a reg " = call i64 @const_init_int(i64 " dat ")\n"
              (llvm-lst e0 (hash-set env x reg)))
          ]
         [(? symbol?)
          (define str (~a (gensym '@.str.)))
          (define length (+ (string-length (~a dat)) 1))
          (define glob (~a str " = private unnamed_addr constant [" length " x i8] c\"" dat "\\00\", align 8"))
          (set! const-lst (cons glob const-lst))
          (~a reg " = call i64 @const_init_symbol(i8* getelementptr inbounds ([" length " x i8], [" length " x i8]* " str ", i32 0, i32 0))\n"
              (llvm-lst e0 (hash-set env x reg)))
          ]
         [(? boolean?)
          (if dat
              (~a reg " = call i64 @const_init_true()\n"
                  (llvm-lst e0 (hash-set env x reg)))
              (~a reg " = call i64 @const_init_false()\n"
                  (llvm-lst e0 (hash-set env x reg))))
          ])
       ]
      [`(clo-app ,x . ,xs)
       (define clo (~a (gensym '%cloptr)))
       (define i0 (~a (gensym '%i0ptr)))
       (define f (~a (gensym '%f)))
       (define fptr (~a (gensym '%fptr)))
       (define void-sig (string-join (map (lambda (x) "i64") (cons x xs)) ","))
       (define void-args (string-join (map (lambda (x) (~a "i64 " (hash-ref env x))) (cons x xs)) ","))
       (~a clo " = inttoptr i64 " (hash-ref env x) " to i64*\n"
           i0 " = getelementptr inbounds i64, i64* " clo ", i64 0\n"
           f " = load i64, i64* " i0 ", align 8\n"
           fptr " = inttoptr i64 " f " to void (" void-sig ")*\n"
           "musttail call fastcc void " fptr "(" void-args ")\n"
           "ret void\n"
           )
       ]
      [`(if ,x ,e0 ,e1)
       (define then (~a (gensym 'label)))
       (define else (~a (gensym 'label)))
       (define cmp (~a "%" (gensym 'cmp)))
       (define false (~a "%" (gensym 'bool)))
       (~a false " = call i64 @const_init_false()\n"
           cmp " = icmp ne i64 " (hash-ref env x) ", " false "\n"
           "br i1 " cmp ",label %" then ", label %" else "\n"
           then ":\n"
           (llvm-lst e0 env)
           else ":\n"
           (llvm-lst e1 env)
           )
       ]))

  (define body (foldr (lambda (x a) (string-append a (llvm-lst x (hash)))) "" proc))
  (define main (~a (string-join const-lst "\n")
                   "\n\n"
                   "define i32 @main() {\n"
                   "call fastcc void @proc_main()\n"
                   "ret i32 0\n"
                   "}\n\n"))
  (string-append main body))





