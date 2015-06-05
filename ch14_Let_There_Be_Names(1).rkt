#lang racket

(define atom? (lambda (a)  (not (list? a))))

(define leftmost-v1
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

; 增加可以處理 null
(define leftmost-v2
  (lambda (l)
    (cond
      ((null? (car l)) '())
      ((atom? (car l)) (car l))
      (else 
       (cond
         ((atom? (leftmost (car l))) (leftmost (car l)))
         (else (leftmost (cdr l))))))))

(define leftmost
  (lambda (l)
    (cond
      ((null? (car l)) '())
      ((atom? (car l)) (car l))
      (else
       (let ((a (leftmost (car l))))
         (cond
           ((atom? a) a)
           (else (leftmost (cdr l)))))))))
        
(leftmost '(() a))

(define eqlist?
  (lambda (al bl)
    (cond
      ((and (null? al) (null? bl)) #t)
      ((null? al) #f)
      ((null? bl) #f)
      ((eq? (car al) (car bl)) (eqlist? (cdr al) (cdr bl)))
      (else #f))))
     
(define rember1*-not-follow-12th-commandment
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (cdr l))
         (else (cons (car l) (rember1* a (cdr l))))))
      (else
       (cond
         ((eqlist? (car l) (rember1* a (car l))) (cons (car l) (rember1* a (cdr l))))
         (else (cons (rember1* a (car l)) (cdr l))))))))

(define rember1*-not-use-let
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                   ((eq? a (car l)) (cdr l))
                   (else (cons (car l) (R (cdr l))))))
                (else
                 (cond
                   ((eqlist? (car l) (R (car l))) (cons (car l) (R (cdr l))))
                   (else (cons (R (car l)) (cdr l)))))))))
      (R l))))

; let ... (R (car l))
(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                   ((eq? a (car l)) (cdr l))
                   (else (cons (car l) (R (cdr l))))))
                (else
                 (let 
                     ((av (R (car l))))
                   (cond
                     ((eqlist? (car l) av) (cons (car l) (R (cdr l))))
                     (else (cons av (cdr l))))))))))
      (R l))))

(rember1* 'a '(('() a b) a (a b c)))

  
  
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else (max (add1 (depth* (car l))) (depth* (cdr l)))))))
           
(depth* '(1(2(3(4(5))))))
(depth* '((1 2 3 (6 7))))
(depth* '())
(depth* '(c (b (a b) a) a))


; 用 let 改寫 scramble

(define pick
  (lambda (n lat)
    (letrec
     ((one?
       (lambda (n) (= 1 n))))
     (cond 
       ((one? n) (car lat))
       (else (pick (sub1 n) (cdr lat)))))))

(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
             (cond
              ((null? tup) '())
              (else
               (let ((rp (cons (car tup) rp)))
                 (cons 
                  (pick (car tup) rp)
                  (P (cdr tup) rp))))))))
      (P tup '()))))  
               
;;; should be '(1 1 1 1 1 1 1 1 1)
(scramble '(1 2 3 4 5 6 7 8 9))

;;; should be '(1 1 1 1 1 1 1 1 2 8 2)
(scramble '(1 2 3 1 2 3 4 1 8 2 10))

