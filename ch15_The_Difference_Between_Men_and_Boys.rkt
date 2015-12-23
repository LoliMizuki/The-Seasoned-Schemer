#lang racket

(define x 'iiooii)
(set! x 'pizza)

(define gourmet
  (lambda (food)
    (cons food (cons x '()))))

(define gourmet-2
  (lambda (food)
    (set! x food) ; 修改 x 的值, 但是仍需事先定義
    (cons food (cons x '()))))

(define dinnerR
  (lambda (food)
    (set! x food)
    (cons 'Milkshake (cons food '()))))

(define omnivore
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define food 'onion)

; swap x and food
(define chez-nous
  (lambda ()
    (let ((temp food))
      (set! food x)
      (set! x temp))))

(chez-nous)
x
food