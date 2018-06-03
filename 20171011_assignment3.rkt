#lang planet neil/sicp
;201312845 김일식

; Lambda Abstraction
(define (aver x y)
  ((lambda (x y) (/ (+ x y) 2)) x y))

(define (doub x)
  ((lambda (x) (+ x x)) x))

(define (fiv)
  ((lambda () 5)))

; Let Binding
(define (foo x y)
  (let ((p (* x y))
        (s (+ x y)))
        (if (even? p)
            (- p s)
            (* s s))))

; Composition of Functions
(define (compose f g)
  (lambda (x) (f (g x))))

; Dotted-Tail Notation
(define (average . l)
  (if (= 0 (length l)) 0
      (/ (apply + l) (length l))))

; Duplicated Elements
(define (duplicate n e)
  (if (= n 0) nil
      (cons e (duplicate (- n 1) e))))
  

