;; Num.1
(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex x)
     (let ((x0 x))
       (write 'x)
       (display " => ")
       (write x0)
       (newline)
       x0))))

;; Num.2
(define-syntax test
  (syntax-rules ()
    ((test action x)
     (list (quote action) x))))

(define (run-test test)
  (display (car test))
  (define res (eval (car test) (interaction-environment)))
  (if (equal? res (cadr test))
      (begin
        (display " ok") (newline)
        #t)
      (begin
        (display " FAIL") (newline)
        (display "  Expected: ") (write (cadr test)) (newline)
        (display "  Returned: ") (write res) (newline)
        #f)))

(define (run-tests list_tests)
  (define (is_tr success? ls)
    (if (null? ls)
        success?
        (if (equal? success? #f)
            (begin
              (run-test (car ls))
              (is_tr success? (cdr ls)))
            (is_tr (and success? (run-test (car ls))) (cdr ls)))))
  (is_tr #t list_tests))

;; Num.3
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
         (and (size-up-ind? (vector->list ls_x) ind)
              (write (vector-ref ls_x ind))))
        
        ((string? ls_x)
         (and (size-up-ind? (string->list ls_x) ind)
              (write (string-ref ls_x ind)))))

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
              (list->string (vector->list
                             (in_x (list->vector (string->list ls_x))
                                   ind (car num)))))))))

;; num.4
(define (factorize ls)
  (let ((a (cadr (cadr ls)))
        (b (cadr (caddr ls))))
    (if (equal? (caddr (cadr ls)) 2)
        (list '*
              (list '- a b)
              (list '+ a b))
        (if (equal? '- (car ls))
            (list '*
                  (list '- a b)
                  (list '+
                        (list 'expt a '2)
                        (list '* a b)
                        (list 'expt b '2)))
            (list '*
                  (list '+ a b)
                  (list '+
                        (list 'expt a '2)
                        (list '- (list '* a b))
                        (list 'expt b '2)))))))

(load "unit-test.scm")
(define the-tests
  (list (test (factorize '(- (expt x 2) (expt y 2)))
              '(* (- x y) (+ x y)))
        (test (factorize '(- (expt (+ first 1) 2)
                             (expt (- second 1) 2)))
              '(* (- (+ first 1) (- second 1))
                  (+ (+ first 1) (- second 1))))
        (test (factorize '(+ (expt x 3) (expt y 3)))
              '(* (+ x y)
                  (+ (expt x 2) (- (* x y)) (expt y 2))))
        (test (factorize '(- (expt x 3) (expt y 3)))
              '(* (- x y)
                  (+ (expt x 2) (* x y) (expt y 2))))))

(run-tests the-tests)