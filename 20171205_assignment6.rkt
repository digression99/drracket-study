#lang planet neil/sicp
;201312845김일식

; generator
(define (inc val)
  (+ val 1))

(define (gen x proc)
  (let
      ((reset-val x))
    (define
      (dispatch tag)
      (cond
        ((eq? tag 'value)
             x)
            ((eq? tag 'next)
             (begin
               (set! x (proc x))
               x))
            ((eq? tag 'reset)
             (set! x reset-val))
            (else
             (error "Unknown Request !"))))
  dispatch))

; iterator
(define (gen-iterator gen)
  (define (gen-iter-helper g cnt n)
    (if (< cnt n)
        (cons (g 'next)
              (gen-iter-helper g (+ cnt 1) n))
        '()))
  (define (dispatch tag)
    (cond
      ((number? tag)
       (gen-iter-helper gen 0 tag))
      ((eq? tag 'reset)
       (gen 'reset))
      (else
       (error "Unknown Request !"))))
  dispatch)
       
  