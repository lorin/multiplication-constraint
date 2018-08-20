#lang racket
(require minikanren)

(define nullo
  (lambda (x) (= '() x)))

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
     (else
      (fresh (d)
             (cdro l d)
             (membero x d))))))


(define digits '(0 1 2 3 4 5 6 7 8 9))

(run 1 (q)
     (fresh (a b c d)
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
            (== q `(,a,b,c,d))
     ))