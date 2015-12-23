#lang racket

(define last 'angelfood)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food (cons 'cake '()))))

(sweet-toothL 'chocolate)
last

(sweet-toothL 'fruit)
last

(displayln '==)

(define ingredients '())

(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food ingredients)
    (sweet-toothL food)))

(sweet-toothR 'chocolate)
(sweet-toothR 'fruit)
(sweet-toothR 'chez)

ingredients

(displayln '==)

(define deep
  (lambda (m)
    (cond 
      ((zero? m) 'pizza)
      (else (cons (deep (sub1 m)) '())))))

(deep 3)

(define Ns '())
(define Rs '())

(define deepR
  (lambda (n)
    (let ((result (deep n)))
      (set! Ns (cons n Ns))
      (set! Rs (cons result Rs))
      result)))

(deepR 3)
Ns
Rs

(deepR 5)
Ns
Rs

(cdr Ns)

(define find
  (lambda (n Ns Rs)
    (letrec ((A (lambda (Ns Rs)
                  (cond
                    ((null? Ns) #f)
                    ((null? Rs) #f)
                    ((= n (car Ns)) (car Rs))
                    (else (find n (cdr Ns) (cdr Rs)))))))
      (A Ns Rs))))

(display '"(find 3 Ns Rs) =") 
(find 3 Ns Rs)

(display '"(find 5 Ns Rs) =") 
(find 5 Ns Rs)

(define member?
  (lambda (a list)
    (cond
      ((null? list) #f)
      ((eq? (car list) a) #t)
      (else (member? a (cdr list))))))

(define deepM
  (lambda (n)
    (if (member? n Ns) (find n Ns Rs) (deepR n))))

(deepM 2)

(define deepM-withoutDeepR
  (lambda (n)
    (if (member? n Ns) (find n Ns Rs)
        (let ((result (deep n)))
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          (result)))))

(deepM-withoutDeepR 1)