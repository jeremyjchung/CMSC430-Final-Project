(define a (hash))
(define b (hash-set a 1 10))
(define c (hash-remove b 1))
(define d (hash-set c 2 10))
(hash-ref d 2)
