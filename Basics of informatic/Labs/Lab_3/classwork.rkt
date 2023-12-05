(load "trace.scm")
(load "unit-test.scm")

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

;;;

(define counter
  (let ((n 0))
    (lambda ()
      (set! n (+ 1 n))
      n)))

(+ (trace-ex (counter)) (trace-ex (counter)))
; (counter) => 1
; (counter) => 2
; 3

(define counter-tests
  (list (test (counter) 3)
        (test (counter) 7) ; !!!
        (test (counter) 5)))

(run-tests counter-tests)
; (counter) ok
; (counter) FAIL
;   Expected: 7
;   Returned: 4
; (counter) ok