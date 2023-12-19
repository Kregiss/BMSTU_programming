;; num.1
(define call/cc call-with-current-continuation)
(define env #f)

(define-syntax use-assertions
  (syntax-rules ()
    ((use-assertations)
     (call/cc (lambda (cont) (set! env cont))))))

(define-syntax assert
  (syntax-rules()
    ((assert expr?)
     (if (not expr?)
         (begin
           (display "FAILED: ")
           (write 'expr?)
           (newline)
           (env))))))

(use-assertions)

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

;; num.3
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

;; num.4
(define-syntax my-if
  (syntax-rules ()
    ((_ term expr-1 expr-2)
     (force (or (and term (delay expr-1))
                (delay expr-2))))))

;; num.5
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var1 val1) ...) expr1 ...)
     ((lambda (var1 ...) expr1 ...) val1 ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () expr1 ...)
     (my-let () expr1 ...))
    ((my-let* ((var1 val1)) expr1 ...)
     (my-let ((var1 val1)) expr1 ...))
    ((my-let* ((var1 val1) (var2 val2) ...) expr1 ...)
     (my-let ((var1 val1))
             (my-let* ((var2 val2) ...) expr1 ...)))))

;; num.6

; A (when, unless)
(define-syntax when
  (syntax-rules()
    ((when cond? expr1 ...)
     (if cond?
         (and expr1 ...)))))

(define-syntax unless
  (syntax-rules()
    ((unless cond? expr1 ...)
     (if (not cond?)
         (and expr1 ...)))))
; Б (for)
(define-syntax for
  (syntax-rules(in as)
    ((for x in xs expr1 ...)
     (let loop ((vals xs))
        (if (not (null? vals))
            (let ((x (car vals)))
              expr1 ...
              (loop (cdr vals))))))
    ((for xs as x expr1 ...)
     (for x in xs expr1 ...))))

; В (while)
(define-syntax while
  (syntax-rules()
    ((while cond? . expr-n)
     (let lambda ()
       (if cond?
           (begin
             (begin . expr-n)
             (lambda)))))))

; Г (repeat..until)
(define-syntax repeat
  (syntax-rules(until)
    ((repeat (expr ...) until cond?)
     (let lambda ()
        expr ...
        (if (not cond?)
            (lambda))))))

; Д (Вывод «в стиле С++»)
(define-syntax cout
  (syntax-rules(<< endl)
    ((cout << endl)
     (newline))
    ((cout << expr)
     (display expr))
    ((cout << expr . other)
     (begin
       (cout << expr)
       (cout . other)))
    ((cout << endl . other)
     (begin
       (cout << endl)
       (cout . other)))))
