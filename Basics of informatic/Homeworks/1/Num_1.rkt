;; Num.1 (Определение дня недели по дате)
; c присваиванием
(define (int_div x y)
  (- (/ x y) (/ (remainder x y) y)))

(define (day-of-week day month year)
  (let* ((a (int_div (- 14 month) 12))
         (y (- year a))
         (m (+ month (* 12 a) -2)))
    (remainder (+ 7000 day y (int_div y 4) (- (int_div y 100)) (int_div y 400) (int_div (* 31 m) 12)) 7)))

; без присваивания (?)
(define (day-of-week-2 day month year)
  (remainder (+ 7000
             day
             (- year (int_div (- 14 month) 12))
             (int_div (- year (int_div (- 14 month) 12)) 4)
             (- (int_div (- year (int_div (- 14 month) 12)) 100))
             (int_div (- year (int_div (- 14 month) 12)) 400)
             (int_div (* 31 (+ month (* 12 (int_div (- 14 month) 12)) -2)) 12))
          7))

;(day-of-week 04 12 1975)  => 4
;(day-of-week 04 12 2006)  => 1
;(day-of-week 29 05 2013)  => 3

;; Num.2 (Действительные корни квадратного уравнения)
(define (square-equation a b c)
  (cond
    ((and (= a 0) (= b 0)) (list))
    ((= a 0) (list (/ (- c) b)))
    (else (let ((d (- (* b b) (* 4 a c))))
            (cond
              ((< d 0) (list))
              ((= d 0) (list (/ (- b) (* 2 a))))
              (else (list (/ (- (- b) (sqrt d)) (* 2 a)) (/ (+ (- b) (sqrt d)) (* 2 a)))))))))


;(square-equation 1 2 1)    => (-1)
;(square-equation 3 -4 -7)  => (-1 2 1/3)

;; Num.3
; НОД
(define (my-gcd a b)
  (if (= a b)
      (abs a)
      (if (> (abs a) (abs b))
          (my-gcd (- (abs a) (abs b)) (abs b))
          (my-gcd (abs a) (- (abs b) (abs a))))))

; НОК
(define (my-lcm a b)
  (/ (* a b) (my-gcd a b)))

; Простота
(define (! x)
  (if (<= x 0)
      1
      (* x (! (- x 1)))))

(define (prime? n)
  (= (remainder (+ 1 (! (- n 1))) n) 0))
