(define a (make-vector 10 1))
(define b 5)
(define c (vector-ref a b))
(vector-set! a (+ c 9) b)
(vector-ref a (+ c 8))
