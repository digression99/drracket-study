#lang planet neil/sicp
(define a 3)
(define (sum a b)(+ a b))
(define (abs x) (
                 if (< x 0) (- x) x))

(define (print-abs x) (
                       if (< x 0) (begin
                                    (display "hi ")
                                    (- x))
                          (begin
                            (display "fuck ")
                            x)))
