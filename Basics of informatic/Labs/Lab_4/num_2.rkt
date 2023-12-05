;; num.2
(define (save-data ls way-to-file)
  (call-with-output-file way-to-file
    (lambda (port)
      (write ls port))))

(define (load-data way-to-file)
  (call-with-input-file way-to-file
    (lambda (port)
      (read port))))

(define (count-lines file)
  (call-with-input-file file
    (lambda (port)
      (define s1 "")
      (define s2 "")
      (define (read-loop count)
        (set! s1 s2)
        (set! s2 (read-char port))
        (if (eof-object? s1)
            count
            (if (or (and (equal? s2 #\return)
                         (not (equal? s1 #\newline)))
                    (and (equal? s2 #\newline)
                         (not (equal? s1 #\newline))
                         (not (equal? s1 #\return)))
                    (and (eof-object? s2)
                         (not (equal? s1 #\newline))))
                (read-loop (+ 1 count))
                (read-loop count))))
      (read-loop 0))))