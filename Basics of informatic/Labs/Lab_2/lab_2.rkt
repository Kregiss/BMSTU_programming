;; Num.1
(define (count x xs)
  (if (null? xs)
      0
      (+ (if (eqv? (car xs) x)
             1
             0)
         (count x (cdr xs)))
      ))

;; Num.2
(define (delete pred? xs)
  (if (= (length xs) 0)
      '()
      (if (pred? (car xs))
          (delete pred? (cdr xs))
          (cons (car xs) (delete pred? (cdr xs))))
      ))

;; Num.3 имеет O(n^2)
(define (iterate f x n)
  (if (> n 0)
      (cons x (map f (iterate f x (- n 1))))
      '()
      ))

;; Num.4
(define (intersperse e xs)
  (if (> (length xs) 1)
      (cons (car xs) (cons e (intersperse e (cdr xs))))
      xs
      ))

;; Num.5  имеет O(n^2) (из-за length)
(define (any? pred? xs)
  (and (> (length xs) 0) (or (pred? (car xs)) (any? pred? (cdr xs)))))

(define (all? pred? xs)
  (or (= (length xs) 0) (and (pred? (car xs)) (all? pred? (cdr xs)))))

;; Num.6
(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

(define (o . xs)
  (if (null? xs)
      (lambda (x) x)
      (lambda (x) ((car xs) ((apply o (cdr xs)) x)))))