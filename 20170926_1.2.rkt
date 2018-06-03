#lang planet neil/sicp
; factorial recursive process
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; factorial iterative process
(define (factorial2 n)
  (fact-iter 1 1 n))

; it's okay if the function is after it's use.
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product) (+ counter 1) max-count)))

; tree recursion, fibonacci
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (fib2 n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0) b
      (fib-iter (+ a b) a (- count 1))))

; counting change
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1)) ; you keep the amount.
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins))))) ; you keep the coin kinds.

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; exponentiation
(define (expt b n)
  (if (= n 0) 1
      (* b (expt b (- n 1))))) ; one by one.

(define (expt2 b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0) product
      (expt-iter b (- counter 1) (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (square n) (* n n)) ; mul is not n * n but (* n n)
; n * n only return n
(define (even? n) (= (remainder n 2) 0))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))




