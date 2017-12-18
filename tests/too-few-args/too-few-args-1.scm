(define a (lambda (a b c) (cons (+ a b) c)))
(define b (a 1 2 '(3 4 5 6)))
(cons (a 0) b)
