(define-syntax my-if
  (syntax-rules ()
    ((_ term expr-1 expr-2)
     (force (or (and term (delay expr-1))
                (delay expr-2))))))

;(my-if #t 1 (/ 1 0)) => 1
;(my-if #f (/ 1 0) 1) => 1
;(my-if #t #f 'a)     => #f

(define counter
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        n)))
(my-if (= (counter) 2)
       'ok
       'fail)
;#fail