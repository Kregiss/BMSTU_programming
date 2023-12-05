(define (run-test test)
  (display (car test))
  (define res (eval (car test) (interaction-environment))) ;; из 8 лекции - выполнение выражения, записанного в цитатах (enva...) - область значений (а именно текущие глобальные переменные среды)
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
  (define (is_tr b ls)
    (if (null? ls)
        b
        (if (equal? b #f)
            (begin
             (run-test (car ls))
             (is_tr b (cdr ls)))
            (is_tr (and b (run-test (car ls))) (cdr ls)))))
  (is_tr #t list_tests))

;;
(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1)
    (else     1)))

(load "unit-test.scm")

(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))

(run-tests the-tests)