#lang planet neil/sicp
(eq? '1 1) ;true
(string? "abc") ;true
(string? 'abc) ;false
(symbol? 'abc) ;true
(eq? (list 'a 'b) '(a b)) ;false
(pair? (list 'a 'b)) ;true
(eq? '(a) '(a)) ;false
(equal? 1 1) ;true
(equal? '1 '1) ;true
(equal? (list 'a 'b 'c) (list 'a 'b 'c)) ;true
(equal? (list 'a 'b 'c) (list 'a 'b 'c 'd)) ;false
(eq? (list 'a 'b 'c) (list 'a 'b 'c)) ;false

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; complex number operation

; z comes with pair.
; with 'rectangular, car z is x, cdr z is y.
; with 'polar, car z is magnitude, cdr z is angle.
(define (make-complex tag z)
  ; let A, r needed.
  (let ((real
         (cond
           (eq? tag 'rectangular) (car z)
           (eq? tag 'polar) (* r (cos (cdr z)))))
        (imag
         (cond
           (eq? tag 'rectangular) (cdr z)
           (eq? tag 'polar) (* r (sin (cdr z)))))
        (mag
         (cond
           (eq? tag 'rectangular) (sqrt (+ (square (car z)) (square (cdr z))))
           (eq? tag 'polar) (car z)))
        (ang
         (cond
           (eq? tag 'rectangular) (atan (cdr z) (car z))
           (eq? tag 'polar) (cdr z))))
    (define (dispatch tag)
      (cond ((eq? tag 'real) real)
            ((eq? tag 'imag) imag)
            ((eq? tag 'mag) mag)
            ((eq? tag 'ang) ang)
            (else
             (error "no data."))))
    dispatch))
    


(define (make-from-real-imag z1 z2))

(define (make-from-mag-ang z1 z2))


(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))




        