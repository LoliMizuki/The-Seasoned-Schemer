#lang racket

;;; two-in-a-rows?

(define is-first?
  (lambda (a lat)
    (cond 
      ((null? lat) #f)
      (else (eq? a (car lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond 
      ((null? lat) #f)
      (else
       (or 
       (eq? a (car lat))
       (two-in-a-rows? lat))))))

(define two-in-rows-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else 
       (or 
        (eq? preceding (car lat))
        (two-in-rows-b? (car lat) (cdr lat)))))))

(define two-in-a-rows?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-rows-b? (car lat) (cdr lat))))))
   
;(two-in-a-rows? '(z x a a b d f))
;(two-in-a-rows? '(z x a b d f))

; sun-of-prefixes
;; sum of numbers seen so far => sonssf

(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      ((null? tup) '())
      (else (cons 
             (+ sonssf (car tup)) 
             (sum-of-prefixes-b (+ sonssf (car tup)) (cdr tup)))))))

(define sum-of-prefixes
  (lambda (tup) (sum-of-prefixes-b 0 tup)))

; scramble
;;; 實際意義?
;;; 因為看不懂作用, 所以用例子
;;; tup = '(1 2 2 3)
;;; rev-pre = '()
;;; result = '()
;;;
;;; 動作: 
;;; (car tup) 得 v
;;; (cons v rev-pre)
;;; 用 v 作 index 取出 rev-pre[v]
;;; rev-pre[v] cons 到 result
;;;
;;; #1: 
;;; - tup = '(1 2 2 3)
;;; - v = (car tup) = 1
;;; - rev-pre = '(1)
;;; - rev-pre[1] = 1
;;; - result = '(1)
;;; 
;;; #2: 
;;; - tup = '(2 2 3)
;;; - v = 2
;;; - rev-pre = '(2 1)
;;; - rev-pre[2] = 1
;;; - result = '(1 1) 
;;; 
;;; #3: 
;;; - tup = '(2 3)
;;; - v = 2
;;; - rev-pre = '(2 2 1)
;;; - rev-pre[2] = 2
;;; - result = '(1 1 2)
;;; 
;;; #4: 
;;; - tup = '(3)
;;; - v = 3
;;; - rev-pre = '(3 2 2 1)
;;; - rev-pre[3] = 2
;;; - result = '(1 1 2 2) 

(define one? (lambda (n) (= 1 n)))

(define pick 
  (lambda (n lat)
    (cond 
      ((one? n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

;;; rev-pre: reversed prefix
(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else 
       (cons 
        (pick (car tup) (cons (car tup) rev-pre))
        (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))

(define scramble (lambda (tup) (scramble-b tup '())))

;; test
;;; should be '(1 1 1 1 1 1 1 1 1)
(scramble '(1 2 3 4 5 6 7 8 9))

;;; should be '(1 1 1 1 1 1 1 1 2 8 2)
(scramble '(1 2 3 1 2 3 4 1 8 2 10))