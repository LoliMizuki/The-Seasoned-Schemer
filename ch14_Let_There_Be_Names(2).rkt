#lang racket

(require mzlib/defmacro)
(define-macro (letcc c . body) 
  `(call/cc (lambda (,c) ,@body)))

(define-macro (try x a b)
  `(letcc *success*
     (letcc ,x
       (*success* ,a))
     ,b))

; (let ()
;   (...)
;   (...))
;
; (letrec ()
;   (...)
;   (...))
;
; (begin
;   (...)
;   (...))

(define atom? (lambda (a) (if (list? a) #f #t)))

(define leftmost
  (lambda (l)
   (letcc skip
      (letrec
          ((lm (lambda (l)
                 (cond
                   ((null? l) '())
                   ((atom? (car l)) (skip (car l)))
                   (else (let ()
                           (lm (car l))
                           (lm (cdr l))))))))
        (lm l)))))
       
       