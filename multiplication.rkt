#lang racket
(require minikanren)
(require minikanren/numbers)

(define nullo
  (lambda (x) (== '() x)))

(define caro
  (lambda (p a)
    (fresh (d)
           (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
           (== (cons a d) p))))
  
(define eq-caro
  (lambda (l x)
    (caro l x)))

(define succeed (== #f #f))
 
(define fail (== #f #t))

(define membero
  (lambda (x l)
    (conde
     ((nullo l) fail)
     ((eq-caro l x) succeed)
     (succeed 
      (fresh (d)
             (cdro l d)
             (membero x d))))))

(define digits '(0 1 2 3 4 5 6 7 8 9))

(run 1 (q)
     (fresh (a b c d tenc)
            (numbero a)
            (numbero b)
            (numbero c)
            (numbero d)
            (membero a digits)
            (membero b digits)
            (membero c digits)
            (membero d digits)
            (=/= a b)
            (=/= a c)
            (=/= a d)
            (=/= b c)
            (=/= b d)
            (=/= c d)
            (*o (build-num 10) (build-num 3) tenc)
            (== q tenc)
            ; (== (* d (+ (* 1000 a) (* 100 b) (* 10 c) d)) (+ (* 1000 d) (* 100 c) (* 10 b) a))
                         
            ; (== q `(,a,b,c,d))
     ))
