(define (o . xs)
  (if (null? xs)
      (lambda (x) x)
      (lambda (x) ((car xs) ((apply o (cdr xs)) x)))))
