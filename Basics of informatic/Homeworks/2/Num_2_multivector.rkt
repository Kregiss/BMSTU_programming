(define (make-multi-vector sizes . fill)
  (if (null? fill)
      (list 'multi-vector sizes (make-vector (apply * sizes)))
      (list sizes (make-vector (apply * sizes) (car fill)))))

(define (multi-vector? m)
  (and (list? m) (equal? 'multi-vector (car m)) (list? (car (cdr m))) (vector? (cadr (cdr m)))))

(define (search-element ls indices)
  (if (equal? (car ls) 'multi-vector)
      (search-element (cdr ls) indices)
      (if (= 1 (length indices))
          (car ls)
          (+ (* (apply * (cdr ls)) (car indices))
             (search-element (cdr ls) (cdr indices))))))
      
(define (multi-vector-ref m indices)
  (if (equal? (car m) 'multi-vector)
      (multi-vector-ref (cdr m) indices)
      (vector-ref (cadr m)
                  (search-element (car m) indices))))

(define (multi-vector-set! m indices x)
  (if (equal? (car m) 'multi-vector)
      (multi-vector-set! (cdr m) indices x)
      (vector-set! (cadr m)
                   (search-element (car m) indices)
                   x)))
