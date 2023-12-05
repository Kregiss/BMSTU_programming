; (trib_n 30)

(define (trib_n n)
  (cond
    ((<= n 1) 0)
    ((= n 2) 1)
    (else (+ (trib_n (- n 3)) (trib_n (- n 2)) (trib_n (- n 1))))))

; с мемоизацией
(define (trib-memo n)
  (let ((memo-ls (make-vector (+ 1 n))))
    (let loop ((n n))
      (cond
        ((<= n 1) 0)
        ((= n 2) 1)
        (else (if (= 0 (vector-ref memo-ls n))
                  (vector-set! memo-ls n (+ (loop (- n 3))
                                         (loop (- n 2))
                                         (loop (- n 1)))))
                  (vector-ref memo-ls n))))))