#lang planet neil/sicp

(define test1 (cons '() '()))
(set-car! test1 (cons 10 20))
(set-cdr! test1 (cons 30 40))
(display test1)

(define foo
  (lambda (x y)
    (+ x y)))