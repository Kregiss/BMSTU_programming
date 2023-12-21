;O((b - a) / d)
(define (my-range a b d)
  (if (< a b)
      (cons a (my-range (+ a d) b d))
      '()))

;O(len(xs)^3)
(define (my-flatten ls)
  (cond
    ((not (pair? ls)) ls)
    ((null? (car ls)) (my-flatten (cdr ls)))
    ((pair? (car ls)) (append (my-flatten (car ls)) (my-flatten (cdr ls))))
    (else (cons (car ls) (my-flatten (cdr ls))))))

;O(len(xs))
(define (my-element? x xs)
  (and (not (null? xs)) (or (equal? x (car xs)) (my-element? x (cdr xs)))))

;O(len(xs))
(define (my-filter pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (cons (car xs) (my-filter pred? (cdr xs)))
          (my-filter pred? (cdr xs)))))

;O(len(xs))
(define (my-fold-left op xs)
  (if (= 1 (length xs))
      (car xs)
      (my-fold-left op (cons (op (car xs) (cadr xs)) (cddr xs)))))


;O(len(xs))
(define (my-fold-right op xs)
  (cond
    ((= 1 (length xs)) (car xs))
    ((= 2 (length xs)) (op (car xs) (cadr xs)))
    (else (op (car xs) (my-fold-right op (cdr xs))))))