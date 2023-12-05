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