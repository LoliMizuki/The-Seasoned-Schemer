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
      (cond
        ((null? set2) '())
        (else (I set1))))))

(intersect '(1 2 3 4) '(3 5 1 8))
(intersect '(1 7 2 3) '(2 4 7 6))

(displayln '---)

(define intersect-all
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
              (cond
                ((null? (cdr lset)) (car lset))
                (else 
                 (intersect 
                  (car lset) 
                  (A (cdr lset))))))))
      (cond
        ((null? lset) '())
        (else (A lset))))))

(intersect-all '())
(intersect-all '((1 7 2 3) (2 4 7 6) (7 1 3 5)))
(intersect-all '((1 7 2 3) () (7 1 3 5)))

(displayln '---)

;; 利用 call-with-current-continuation (可簡寫為 call/cc)
;; 將目前的計算狀態保存於某一變數中傳入
;; 例如: 下例中的 hop 
;; 發現輸入的 sets 有 '() 者, 立刻回傳解答
(define intersect-all-with-hop
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                   ; 發現 '() -> 立即結束
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

(displayln '---)

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


; 可以在 intersect 階段就判斷出 null? 並立即回傳
; 合併 intersect 和 intersect-all

(define intersect-all-final
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
           ((A ; intersect-all 的主體
            (lambda (lset)
              (cond
                ((null? (car lset)) (hop '()))
                ((null? (cdr lset)) (car lset))
                (else (I (car lset) (A (cdr lset)))))))
            (I ; intersect
             (lambda (s1 s2)
               (letrec
                   ((J (lambda (s1)
                         (cond
                           ((null? s1) '())
                           ; 這地方怪怪的?
                           ;((member? (car s1) s2)
                           ; (J (cdr s1)))
                           ;(else (cons 
                           ;       (car s1) 
                           ;       (J (cdr s1))))))))
                           ; Miz 修正
                           ((member? (car s1) s2)  
                            (cons (car s1) (J (cdr s1))))
                           (else (J (cdr s1)))))))
                 (cond
                   ((null? s2) (hop '()))
                   (else (J s1)))))))
         (cond
           ((null? lset) '())
           (else (A lset))))))))

(intersect-all-final '())
(intersect-all-final '((1 7 2 3) (2 4 7 6) (7 1 3 5)))
(intersect-all-final '((1 7 2 3) () (7 1 3 5)))

(displayln '---)

; remeber-beyond-first: 移除第一個 a 之後的元素(含 a)

(define rember
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? a (car lat)) (cdr lat))
                (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(rember 'a '(a e r y a))

(define remeber-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
            (cond
              ((null? lat) '())
              ((eq? a (car lat)) '())
              (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(remeber-beyond-first 'h '(a b c d e f g h i j k l))

(displayln '---)

; remeber-upto-last: 移除最後一個 a 之前的元素(含 a)
(define remeber-upto-last
  (lambda (a lat)
    (call/cc
     (lambda (skip)
       (letrec
           ((R (lambda (lat)
                 (cond
                   ((null? lat) '())
                   ; 當發現 a 時, 忽略目前結果, 執行 (R (cdr lat))
                   ((eq? (car lat) a) (skip (R (cdr lat)))) 
                   (else (cons (car lat) (R (cdr lat))))))))
         (R lat))))))

(remeber-upto-last 'o '(a b c o d e f g o i j o k l))
; should be k l

