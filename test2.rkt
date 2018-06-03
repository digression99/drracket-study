#lang planet neil/sicp
(define (factorial n)
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
  (fact-iter 1 1 n))

(define (sqrt x)
  (define (average x y) (/ (+ x y) 2))
  (define (square x) (* x x))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define two-sum-vec (vector))
(define (two-sum vec tgt)
  (





  