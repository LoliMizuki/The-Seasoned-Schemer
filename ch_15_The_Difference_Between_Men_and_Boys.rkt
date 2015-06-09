#lang racket

(define x 'iiooii)
(set! x 'pizza)

(define gourmet
  (lambda (food)
    (cons food (cons x '()))))