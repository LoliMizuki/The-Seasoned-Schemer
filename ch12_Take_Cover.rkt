#lang racket

;(define multirember
;  (lambda (a lat)
;    (cond
;      ((null? lat) '())
;      ((eq? a (car lat)) (multirember a (cdr lat)))
;      (else (cons (car lat) (multirember a (cdr lat)))))))

; (multirember 'a '(c v a b a))

;(define length
;  (Y (lambda (length)
;       (lambda (l)
;         (cond
;           ((null? l) 0)
;           (else (add1 (length (cdr l)))))))))

(define multirember
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                ((null? lat) '())
                ((eq? a (car lat)) (mr (cdr lat)))
                (else (cons (car lat) (mr (cdr lat))))))))
       mr) lat)))
                
(multirember 'a '(c v a b a))

; letrec 

(define outter
  (lambda (a n)
    ((letrec
         ((inner (lambda (nn) (- a nn)))
          (inner2 (lambda (nn) (+ a nn))))
       inner2) n)))