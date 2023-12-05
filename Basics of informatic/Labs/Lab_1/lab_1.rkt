;; чётное ли число
(define (my-even? x)
  (if (= (remainder x 2) 0) #t #f))

;; нечётное ли число
(define (my-odd? x)
  (if (= (remainder x 2) 0) #f #t))

;; возведение в степень (рекурсия)
(define (power-base-exp x a)
  (if (= a 0)
      1
      (* x (power-base-exp x (- a 1)))))