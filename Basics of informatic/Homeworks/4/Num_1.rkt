;; Num_1
(define (memoized-factorial n)
  (let ((memo-list '()))
    (define (lookup n)
      (cond
       ((null? memo-list) #f)
       ((= (caar memo-list) n) (cdar memo-list))
       (else (lookup n))))

    (define (store n result)
      (set! memo-list (cons (cons n result) memo-list)))

    (define (factorial-memo n)
      (cond
       ((or (= n 0) (= n 1)) 1)
       (else
        (let ((result (lookup n)))
          (if result
              result
              (let ((fact-minus-1 (factorial-memo (- n 1))))
                (store n (* n fact-minus-1))
                (* n fact-minus-1)))))))

    (factorial-memo n)))

;(begin
;  (display (memoized-factorial 10)) (newline)
;  (display (memoized-factorial 50)) (newline))

;; Num_2

(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b)
     (cons a (delay b)))))

(define (lazy-car p)
  (car p))

(define (lazy-cdr p)
  (force (cdr p)))

(define (lazy-head xs k)
  (if (= k 0)
      '()
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))))

(define (lazy-ref xs k)
  (if (= k 0)
      (lazy-car xs)
      (lazy-ref (lazy-cdr xs) (- k 1))))

(define (naturals start)
  (lazy-cons start (naturals (+ start 1))))

(define (factorials)
  (let main_proc ((p 1) (n 1))
    (lazy-cons (* p n) (main_proc (* p n) (+ n 1)))))

(define (lazy-factorial n)
  (list-ref (lazy-head (factorials) n) (- n 1)))

;; Примеры использования
;;(display (lazy-head (naturals 10) 12))
;;(newline)

;;(begin
;;  (display (lazy-factorial 10)) (newline)
;;  (display (lazy-factorial 50)) (newline))