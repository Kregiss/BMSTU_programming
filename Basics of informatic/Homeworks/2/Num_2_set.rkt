; O(len(xs)^2)
(define (list->set xs)
  (cond
    ((null? xs) '())
    ((member (car xs) (cdr xs)) (list->set (cdr xs)))
    (else (cons (car xs) (list->set (cdr xs))))))
;(list->set '(1 1 2 3)) ;=> (3 2 1)

; O(len(xs)^2)
(define (set? xs)
  (or (null? xs) (and (not (member (car xs) (cdr xs))) (set? (cdr xs)))))
;(set? '(1 2 3))   ;=> #t
;(set? '(1 2 3 3)) ;=> #f
;(set? '())        ;=> #t

;O(len(xs) * len(ys))
(define (union xs ys)
  (if (not (= 0 (length xs)))
      (if (member (car xs) ys)
          (union (cdr xs) ys)
          (cons (car xs) (union (cdr xs) ys)))
      ys))
;(union '(1 2 3 5) '(2 3 4)) ;=> (5 4 3 2 1)

;O(len(xs) * len(ys))
(define (intersection xs ys)
  (if (not (= 0 (length xs)))
      (if (member (car xs) ys)
          (cons (car xs) (intersection (cdr xs) ys))
          (intersection (cdr xs) ys))
      '()))
;(intersection '(1 2 3) '(2 3 4))  ;=> (2 3)

;O(len(xs) * len(ys))
(define (difference xs ys)
  (if (not (= 0 (length xs)))
      (if (member (car xs) ys)
          (difference (cdr xs) ys)
          (cons (car xs) (difference (cdr xs) ys)))
      '()))
;(difference '(1 2 3 4 5) '(2 3))  ;=> (1 4 5)
;(difference '(1 2 3 4) '(3 4 5 6))
;(difference '(3 4 5 6) '(1 2 3 4))

;O(len(xs) * len(ys))
(define (symmetric-difference xs ys)
  (union (difference xs ys) (difference ys xs)))
;(symmetric-difference '(1 2 3 4) '(3 4 5 6)) ;=> (6 5 2 1)

;O(len(xs) * len(ys))
(define (set-eq? xs ys)
  (null? (symmetric-difference xs ys)))
;(set-eq? '(1 2 3) '(3 2 1)) ;=> #t
;(set-eq? '(1 2) '(1 3))     ;=> #f
