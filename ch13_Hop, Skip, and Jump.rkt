#lang racket

(define member?
  (lambda (a lat)
    (letrec
         ((yes? (lambda (l)
                 (cond
                   ((null? l) #f)
                   ((eq? (car l) a) #t)
                   (else (yes? (cdr l)))))))
       (yes? lat))))

(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond
                ((null? set) '())
                ((member? (car set) set2) 
                 (cons (car set) (I (cdr set))))
                (else (I (cdr set)))))))
      (I set1))))

(intersect '(1 2 3 4) '(3 5 1 8))
(intersect '(1 7 2 3) '(2 4 7 6))

(define intersect-all
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
              (cond
                ((null? (cdr lset)) (car lset))
                (else 
                 (intersect 
                  (car lset) 
                  (intersect-all (cdr lset))))))))
      (cond
        ((null? lset) '())
        (else (A lset))))))

(intersect-all '())
(intersect-all '((1 7 2 3) (2 4 7 6) (7 1 3 5)))
(intersect-all '((1 7 2 3) () (7 1 3 5)))


;; 利用 call-with-current-continuation
;; 發現輸入的 sets 有 '() 者, 立刻回傳解答
(define intersect-all-with-hop
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                   ((null? (car lset)) (hop '()))
                   ((null? (cdr lset)) (car lset))
                   (else (intersect 
                          (car lset) 
                          (A (cdr lset))))))))
         (cond
           ((null? lset) '())
           (else (A lset))))))))

(intersect-all-with-hop '())
(intersect-all-with-hop '((1 7 2 3) (2 4 7 6) (7 1 3 5)))
(intersect-all-with-hop '((1 7 2 3) () (7 1 3 5)))


; letcc version,  racket cano not do like this?
;(define intersect-all-with-hop
;  (lambda (lset)
;    (letcc hop
;       (letrec
;           ((A (lambda (lset)
;                 (cond
;                   ((null? (car lset)) (hop '()))
;                   ((null? (cdr lset)) (car lset))
;                   (else (intersect 
;                          (car lset) 
;                          (A (cdr lset))))))))
;         (cond
;           ((null? lset) '())
;           (else (A lset)))))))