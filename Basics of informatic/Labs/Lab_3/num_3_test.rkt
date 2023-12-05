(define (size-up? a b)
  (>= (length a) b))

(define (size-up-ind? a b)
  (> (length a) b))

(define (in_x ls ind x)
  (define k (length (vector->list ls)))
  (do ((vec (make-vector (+ k 1)))
        (i 0 (+ i 1)))
     ((= i (+ k 1)) vec)
     (cond
       ((= i ind) (vector-set! vec ind x))
       ((< i ind) (vector-set! vec i (vector-ref ls i)))
       ((> i ind) (vector-set! vec i (vector-ref ls (- i 1)))))))

(define (ref ls_x ind . num)
  (if (null? num)
      
      ;доступ к ind элементу

      (cond
        ((list? ls_x)
         (and (size-up-ind? ls_x ind) (write (list-ref ls_x ind))))
        
        ((vector? ls_x)
         (and (size-up-ind? (vector->list ls_x) ind) (write (vector-ref ls_x ind))))
        
        ((string? ls_x)
         (and (size-up-ind? (string->list ls_x) ind) (write (string-ref ls_x ind)))))

      ;процедура "вставки"
      
      (cond
        ((list? ls_x)
         (and (size-up? ls_x ind)
              (vector->list (in_x (list->vector ls_x) ind (car num)))))
        ((vector? ls_x)
         (and (size-up? (vector->list ls_x) ind)
              (in_x ls_x ind (car num))))
        ((string? ls_x)
         (and (size-up? (string->list ls_x) ind)
              (char? (car num))
              (list->string (vector->list (in_x (list->vector (string->list ls_x)) ind (car num)))))))))

;(ref '(1 2 3) 1)     => 2
;(ref #(1 2 3) 1)     => 2
;(ref "123" 1)        => #\2
;(ref "123" 3)        => #f

;(ref '(1 2 3) 1 0)   => (1 0 2 3)
;(ref #(1 2 3) 1 0)   => #(1 0 2 3)
;(ref #(1 2 3) 1 #\0) => #(1 #\0 2 3)
;(ref "123" 1 #\0)    => "1023"
;(ref "123" 1 0)      => #f
;(ref "123" 3 #\4)    => "1234"
;(ref "123" 5 #\4)    => #f