#lang racket
(require minikanren)
(require minikanren/numbers)

;
; These definitions are taken from The Reasoned Schemer
;

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

;
; These two definitions solve the problem
;

(define from-digits 
  (lambda (a b c d abcd)
    (let ((ten (build-num 10))
          (hundred (build-num 100))
          (thousand (build-num 1000)))
      (fresh (tha hub tec thahub tecd)
        (*o thousand a tha)
        (*o hundred b hub)
        (*o ten c tec)
        (pluso tha hub thahub)
        (pluso tec d tecd)
        (pluso thahub tecd abcd)))))

(define solution
  (let ((digits (map build-num '(0 1 2 3 4 5 6 7 8 9))))
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
      (fresh (abcd dcba abcdtd)
        (from-digits a b c d abcd)
        (from-digits d c b a dcba)
        (*o abcd d abcdtd)
        (== abcdtd dcba)
        (== q abcd))))))

;
; Format the solution as a base 10 number
;

(define to-int
  (lambda (l)
    (cond
      ((null? l) 0)
      (else
       (+ (car l) (* 2 (to-int (cdr l))))))))


(to-int (car solution))
