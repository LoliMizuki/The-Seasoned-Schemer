#lang racket

(define counter 0)

(define consC
  (let ((N 0))
    (set! counter (lambda () N))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define deep
  (lambda (m)
    (cond 
      ((zero? m) 'pizza)
      (else (consC (deep (sub1 m)) '())))))