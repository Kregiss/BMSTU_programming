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