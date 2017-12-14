#lang racket

(require "top-level.rkt")
(require "desugar.rkt")
(require "cps.rkt")
(require "closure-convert.rkt")
(require "utils.rkt")
(provide compile-all)



(define (compile-all e)
  (define ir-0 (top-level e))
  (define ir-1 (simplify-ir (desugar ir-0)))
  (define ir-2 (cps-convert (anf-convert (alphatize (assignment-convert ir-1)))))
  (define ir-3 (proc->llvm (closure-convert ir-2)))
  ir-3
  )