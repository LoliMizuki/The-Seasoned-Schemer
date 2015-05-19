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
    (letrec
        ((mr (lambda (lat)
               (cond
                 ((null? lat) '())
                 ((eq? a (car lat)) (mr (cdr lat)))
                 (else (cons (car lat) (mr (cdr lat))))))))
    (mr lat))))

                
;(multirember 'a '(c v a b a))

; letrec 

(define outter
  (lambda (a n)
    (letrec
         ((inner (lambda (nn) (- a nn)))
          (inner2 (lambda (nn) (+ a nn))))
      (cond 
        ((< n 5) (- a n))
        (else (+ a n))))))

; rember-f

(define rember-ff
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else (cons (car l) (rember-ff test? a (cdr l)))))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

; ((rember-f eq?) 'a '(z b a d r))

; 試寫原本的 multirember-fff
(define multirember-fff
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a) ((multirember-fff test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-fff test?) a (cdr lat))))))))

; ((multirember-f eq?) 'a '(d a f a e a g h))
; ((multirember-f <) 5 '(1 2 3 4 5 6 7 8 9 10))

;;; 利用 letrec 來化簡 重複的 (multirember-f test?)

(define multirember-f
  (lambda (test?)
    (letrec 
        ((m-f
          (lambda (a lat)
            (cond
              ((null? lat) '())
              ((test? (car lat) a) (m-f a (cdr lat)))
              (else (cons (car lat) (m-f a (cdr lat))))))))
      m-f)))
     
;((multirember-f eq?) 'a '(d a f a e a g h))
;((multirember-f <) 5 '(1 2 3 4 5 6 7 8 9 10))

(define member?-a
  (lambda (a lat)
    ((letrec
         ((yes? (lambda (l)
                 (cond
                   ((null? l) #f)
                   ((eq? (car l) a) #t)
                   (else (yes? (cdr l)))))))
       yes?)
     lat)))

; 讓 letrec 的 naming part 和 value part 不會混淆
(define member?
  (lambda (a lat)
    (letrec
         ((yes? (lambda (l)
                 (cond
                   ((null? l) #f)
                   ((eq? (car l) a) #t)
                   (else (yes? (cdr l)))))))
       (yes? lat)))) ; <-- clear !!!

;(member?-a 'a '(e f h a v))
;(member?-a 'a '(e f h v))
;(member? 'a '(e f h a v))
;(member? 'a '(e f h v))

(define union-a
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union-a (cdr set1) set2))
      (else (cons (car set1) (union-a (cdr set1) set2))))))

; (union '(a b c d) '(b d e f))

;; set2 在變化過程中不會變動, 所以慘招 letrec 了 >///<

(define union-b
  (lambda (set1 set2)
    (letrec 
        ((uf (lambda (set) 
               (cond
                 ((null? set) set2)
                 ((member? (car set) set2) (uf (cdr set)))
                 (else (cons (car set) (uf (cdr set))))))))
      (uf set1))))

;(union '(a b c d) '(b d e f))

;; letrec 定義多組 => 本身的 U, 自帶 member?
;;; U  -> union-internal
;;; M? -> member? in union
;;; N? -> member?-internal

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set) 
              (cond
                ((null? set) set2)
                ((M? (car set) set2) (U (cdr set)))
                (else (cons (car set) (U (cdr set)))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (l)
                            (cond
                              ((null? l) #f)
                              ((eq? (car l) a) #t)
                              (else (N? (cdr l)))))))
                 (N? lat)))))

      (U set1))))

(union '(a b c d) '(b d e f))

; 改寫 two-in-a-rows?
;; 先定義 letrec, 再 two-in-a-rows 的 lambda

(define two-in-a-rows?  
    (letrec 
        ((W (lambda (a lat)
              (cond
                ((null? lat) #f)
                (else 
                 (or (eq? a (car lat)) (W (car lat) (cdr lat))))))))
      (lambda (lat)
        (cond
          ((null? lat) #f)
          (else (W (car lat) (cdr lat)))))))


(two-in-a-rows? '())
(two-in-a-rows? '(z x a a b d f))
(two-in-a-rows? '(z x a b a d f))