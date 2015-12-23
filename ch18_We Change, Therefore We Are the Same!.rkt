#lang racket

; kons
; kdr

(define kar (lambda (c) (c (lambda (a d) a))))

(define kdr (lambda (c) (c (lambda (a d) d))))

(define kons
  (lambda (kar kdr) (lambda (selector) (selector kar kdr))))



(define lots
  (lambda (n)
    (cond
      ((zero? n) '())
      (else (cons 'egg (lots (sub1 n)))))))

(define lenkth
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lenkth (cdr l)))))))

(define add-at-end
  (lambda (l)
    (cond
      ((null? (cdr l)) (cons (car l) (cons 'egg '())))
      (else (cons (car l) (add-at-end (cdr l)))))))

;(define add-at-end-too
;  (lambda (l)
    