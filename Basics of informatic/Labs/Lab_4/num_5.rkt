(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var1 val1) ...) expr1 ...)
     ((lambda (var1 ...) expr1 ...) val1 ...))))


; example
(define (f1 x y)
  (my-let ((y x)
           (x y))
          (+ (* 100 x) y)))

(f1 2 3) ; 302


(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () expr1 ...)
     (my-let () expr1 ...))
    ((my-let* ((var1 val1)) expr1 ...)
     (my-let ((var1 val1)) expr1 ...))
    ((my-let* ((var1 val1) (var2 val2) ...) expr1 ...)
     (my-let ((var1 val1))
             (my-let* ((var2 val2) ...) expr1 ...)))))