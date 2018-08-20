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

(define digits (map build-num '(0 1 2 3 4 5 6 7 8 9)))

(define to-int
  (lambda (l)
    (cond
      ((null? l) 0)
      (else
       (+ (car l) (* 2 (to-int (cdr l))))))))

(define ten (build-num 10))
(define hundred (build-num 100))
(define thousand (build-num 1000))

(car (map to-int   
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
    (fresh (tha hub tec thahub tecd abcd
                thd huc teb thdhuc teba dcba
                abcdtd)
      (*o thousand a tha)
      (*o hundred b hub)
      (*o ten c tec)
      (pluso tha hub thahub)
      (pluso tec d tecd)
      (pluso thahub tecd abcd)
      (*o thousand d thd)
      (*o hundred c huc)
      (*o ten b teb)
      (pluso thd huc thdhuc)
      (pluso teb a teba)
      (pluso thdhuc teba dcba)
      (*o abcd d abcdtd)
      (== abcdtd dcba)
      (== q abcd))))))
