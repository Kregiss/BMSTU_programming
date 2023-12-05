; А  (when, unless)
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

; Пусть x = 1
(define x 1)
;(when   (> x 0) (display "x > 0")  (newline))
;(unless (= x 0) (display "x != 0") (newline))

;x > 0
;x != 0

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

;(for i in '(1 2 3)
;  (for j in '(4 5 6)
;    (display (list i j))
;    (newline)))

; В (while)

(define-syntax while
  (syntax-rules()
    ((while cond? . expr-n)
     (let lambda ()
       (if cond?
           (begin
             (begin . expr-n)
             (lambda)))))))

(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))

; Г (repeat..until)
(define-syntax repeat
  (syntax-rules(until)
    ((repeat (expr ...) until cond?)
     (let lambda ()
        expr ...
        (if (not cond?)
            (lambda))))))

(let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))

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

;(cout << "a = " << 1 << endl << "b = " << 2 << endl)

;a = 1
;b = 2

(let ((a 2) (b 3))
  (cout << "a + b = " << ( + a b) << endl))
; a + b = 5  