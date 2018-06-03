#lang planet neil/sicp
;201312845 김일식

; linear recursion
(define (f-rec n)
  (if (< n 2) 1
  (+ (* 3 (f-rec (- n 1))) 5)))

(define (f-iter n)
  (define (f-iter-helper n val)
    (if (< n 2) val
        (f-iter-helper (- n 1) (+ (* 3 val) 5))))
  (f-iter-helper n 1))

; tree recursion
(define (a-rec n)
  (cond ((= n 1) 1)
        ((= n 2) 2)
        (else (+ (- (* 3 (a-rec (- n 2))) (a-rec (- n 1))) 2))))

(define (a-iter n)
  (define (a-iter-helper adprv aprv n cnt) ; aprv : An-1, adprv : An-2
    (cond ((= cnt 1) 1)
          ((= cnt 2) 2)
          ((= n cnt) aprv)
          (else (a-iter-helper aprv (+ (- (* 3 adprv) aprv) 2) (+ n 1) cnt))))
  (a-iter-helper 1 2 2 n))

; binary function
(define (sum-from-to-rec x y)
  (if (= x y) y
      (+ x (sum-from-to-rec (+ x 1) y))))

(define (sum-from-to-iter x y)
  (define (sum-from-to-iter x y sum)
    (cond
      ((= x y) (+ sum y))
      (else (sum-from-to-iter (+ x 1) y (+ sum x)))))
  (sum-from-to-iter x y 0))

; primality test
(define (prime? n)
  (define (check-prime nowNum sqrtn n)
    (cond
      ((< nowNum (+ sqrtn 1))
       (cond
         ((= (floor (/ n nowNum)) (/ n nowNum)) #f)
         (else (check-prime (+ nowNum 1) sqrtn n))))
      (else #t)))
  (check-prime 2 (sqrt n) n))




