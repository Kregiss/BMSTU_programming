(define (make-multi-vector sizes . fill)
  (if (null? fill)
      (list sizes (make-vector (apply * sizes)))
      (list sizes (make-vector (apply * sizes) (car fill)))))
  
; (make-multi-vector '(2 4 6)) => список, где первый элемент - "$", а дальше - вектор из 2*4*6 элементов

;(define (make-multi-vector sizes . fill)
;  (if (null? fill)
;      (if (= 1 (length sizes))
;          (make-vector (car sizes))
;          (list->vector (append (vector->list (make-vector (car sizes))) '($) (vector->list (make-multi-vector (cdr sizes))))))
;      (if (= 1 (length sizes))
;          (make-vector (car sizes) (car fill))
;          (list->vector (append (vector->list (make-vector (car sizes) (car fill))) '($) (vector->list (make-multi-vector (cdr sizes) (car fill))))))))

(define (multi-vector? m)
  (and (list? m) (list? (car m)) (vector? (cadr m))))

(define (search-element ls indices)
  (if (= 1 (length indices))
      (car ls)
      (+ (* (apply * (cdr ls)) (car indices))
         (search-element (cdr ls) (cdr indices)))))
      
(define (multi-vector-ref m indices)
  (vector-ref (cadr m)
              (search-element (car m) indices)))

(define (multi-vector-set! m indices x)
  (vector-set! (cadr m)
               (search-element (car m) indices)
               x))

(define m (make-multi-vector '(11 12 9 16)))
(multi-vector? m) ;=> #t
(multi-vector-set! m '(10 7 6 12) 'test)
(multi-vector-ref m '(10 7 6 12)) ;=> test