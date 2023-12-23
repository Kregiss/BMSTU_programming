(load "unit-test.scm")
(define call/cc call-with-current-continuation)

;; Конструктор потока
(define (make-stream items . eos)
  (if (null? eos)
      (make-stream items #f)
      (list items (car eos))))

;; Запрос текущего символа
(define (peek stream)
  (if (null? (car stream))
      (cadr stream)
      (caar stream)))

;; Продвижение вперёд
(define (next stream)
  (let ((n (peek stream)))
    (if (not (null? (car stream)))
        (set-car! stream (cdr (car stream))))
    n))

; Num_1

; <simple fraction> :: = <decimal-is-integer> 'fraction sign' <decimal-integer-unsigned>
; <decimal-is-integer> :: = <sign> <decimal-integer-unsigned> | <decimal-integer-unsigned>
; <sign> :: = - | +
; <decimal-integer-unsigned> :: = <figure> <decimal-integer-unsigned-ost>
; <decimal-integer-unsigned-ost> :: = <figure> <decimal-integer-unsigned-ost> | <empty>

(define (check-frac str)
  (let* ((EOF (integer->char 0))
         (stream (make-stream (string->list str) EOF)))
    (call/cc
     (lambda (error)
       (chfrac stream error)
       (equal? (peek stream) EOF)))))

; <sign> :: = - | +
(define (sign? a)
  (or (equal? a #\+) (equal? a #\-)))

(define (chfrac stream error)
  ; <decimal-integer-unsigned> :: = <figure> <decimal-integer-unsigned-ost>
  (define (number-without-sign stream error)
    (cond ((char-numeric? (peek stream))
           (next stream)
           (number-without-sign-ost stream error))
          (else (error #f))))
  ; <decimal-integer-unsigned-ost> :: = <figure> <decimal-integer-unsigned-ost> |<empty>
  (define (number-without-sign-ost stream error)
    (cond ((char-numeric? (peek stream))
           (next stream)
           (number-without-sign-ost stream error))
          (else '())))
 
  ; <decimal-is-integer> :: = <sign> <decimal-integer-unsigned> | <decimal-integer-unsigned>
  (define (number stream error)
    (cond ((sign? (peek stream))
           (next stream)
           (number-without-sign stream error))
          ((char-numeric? (peek stream))
           (number-without-sign stream error))
          (else (error #f))))
  ;'fraction sign' 
  (define (sgn stream error)
    (cond ((equal? (peek stream) #\/)
           (next stream))
          (else (error #f))))
  ; <simple fraction> :: = <decimal-is-integer> 'fraction sign' <decimal-integer-unsigned>
  (number stream error)
  (sgn stream error)
  (number-without-sign stream error))

(define (scan-frac str)
  (let* ((EOF (integer->char 0))
         (stream (make-stream (string->list str) EOF)))
    (call/cc
     (lambda (error)
       (define result (frac stream error))
       (and (equal? (peek stream) EOF) result)))))

(define (frac stream error)
  ; <decimal-integer-unsigned> :: = <figure> <decimal-integer-unsigned-ost>
  (define (number-without-sign stream error)
    (cond ((char-numeric? (peek stream))
           (cons (next stream) (number-without-sign-ost stream error)))
          (else (error #f))))
  ; <decimal-integer-unsigned-ost> :: = <figure> <decimal-integer-unsigned-ost> |<empty>
  (define (number-without-sign-ost stream error)
    (cond ((char-numeric? (peek stream))
           (cons (next stream) (number-without-sign-ost stream error)))
          (else '())))
  ; <decimal-is-integer> :: = <sign> <decimal-integer-unsigned> | <decimal-integer-unsigned> 
  (define (number stream error)
    (cond ((sign? (peek stream))
           (cons (next stream) (number-without-sign stream error)))
          ((char-numeric? (peek stream))
           (number-without-sign stream error))
          (else (error #f))))
  ;'fraction sign' 
  (define (sgn stream error)
    (cond ((equal? (peek stream) #\/)
           (next stream))
          (else (error #f))))
  ;результат работы scan-frac
  (define (function sign sp i res)
    (cond ((null? sp) (sign res))
          ((sign? (car sp)) (function (if (equal? (car sp) #\-) - +)
                                      (cdr sp) (- i 1) res))
          
          (else (function sign
                          (cdr sp)
                          (- i 1)
                          (+ res (* (- (char->integer (car sp)) 48) (expt 10 i)))))))

  (let ((numerator (number stream error)) ;numerator
        (div (sgn stream error))
        (denominator (number-without-sign stream error)));denominator
    (/ (function + numerator (- (length numerator) 1) 0)
       (function + denominator (- (length denominator) 1) 0))))
    
  
; <fractions> :: = <whitespace characters> <fractions> | <simple fraction> <fractions> |
;                  <empty>
; <whitespace characters> :: = <SPACE CHARACTER> <whitespace characters> | <empty>
; <simple fraction> :: = <decimal-is-integer> 'fraction sign' <decimal-integer-unsigned>
; <decimal-is-integer> :: = <sign> <decimal-integer-unsigned> | <decimal-integer-unsigned>
; <sign> :: = - | +
; <decimal-integer-unsigned> :: = <figure> <decimal-integer-unsigned-ost>
; <decimal-integer-unsigned-ost> :: = <figure> <decimal-integer-unsigned-ost> | <empty>

(define (scan-many-fracs str)
  (let* ((EOF (integer->char 0))
         (stream (make-stream (string->list str) EOF)))
    (call/cc
     (lambda (error)
       (define result (fracs stream error))
       (and (equal? (peek stream) EOF) result)))))

; <whitespace characters> :: = <SPACE CHARACTER> <whitespace characters> | <empty>
(define (space stream error)
  (cond ((char-whitespace? (peek stream))
         (next stream)
         (space stream error))
        (else #t)))

; <fractions> :: = <whitespace characters> <fractions> |<simple fraction> <fractions> |
;                  <empty>
(define (fracs stream error)
  (cond ((char-whitespace? (peek stream))
         (next stream)
         (fracs stream error))                  
        ((or (sign? (peek stream)) (char-numeric? (peek stream)))
         (cons (frac stream error) (fracs stream error)))
        (else '())))

(define the-tests
  (list (test (check-frac "110/111") '#t)
        (test (check-frac "-4/3") '#t)
        (test (check-frac "+5/10") '#t)
        (test (check-frac "5.0/10") '#f)
        (test (check-frac "FF/10") '#f)
        (test (check-frac "/") '#f)
        (test (check-frac "1/") '#f)
        (test (check-frac "/1") '#f)
        (test (check-frac "") '#f)
        (test (check-frac "+/1") '#f)
        (test (check-frac "+1 1/1") '#f)
        (test (check-frac "") '#f)
        (test (check-frac "+/2") '#f)
        (test (check-frac "2/") '#f)
        (test (check-frac "+1/") '#f)
        (test (check-frac "-2/1") '#t)
        (test (scan-frac "110/111") '110/111)
        (test (scan-frac "-4/3") '-4/3)
        (test (scan-frac "+5/10") '1/2)
        (test (scan-frac "5.0/10") '#f)
        (test (scan-frac "FF/10") '#f)
        (test (scan-frac "") '#f)
        (test (scan-frac "-/") '#f)
        (test (scan-frac "+/2") '#f)
        (test (scan-frac "/") '#f)
        (test (scan-frac "/2") '#f)
        (test (scan-many-fracs "") '())
        (test (scan-many-fracs "\t1/2 1/3\n\n10/8") '(1/2 1/3 5/4))
        (test (scan-many-fracs "1/21/3") #f)
        (test (scan-many-fracs "1/2 1/3") '(1/2 1/3))
        (test (scan-many-fracs "\t1/2 1/3\n\n2/-5") '#f)
        ;(test  '#t)
        
        ))

(run-tests the-tests)

; Num_2

;<Program>  ::= <Articles> <Body> .
;<Articles> ::= <Article> <Articles> | .
;<Article>  ::= define word <Body> end .
;<Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | .

(define (parse vec)
  (let* ((EOF (integer->char 0))
         (stream (make-stream (vector->list vec) EOF))) 
    (call/cc
     (lambda (error)
       (define result (program stream error))
       (and (equal? (peek stream) EOF)
            result)))))

;<Program>  ::= <Articles> <Body> .
(define (program stream error)
  (list (articles stream error)
        (body stream error)))

;<Articles> ::= <Article> <Articles> | .
(define (articles stream error)
  (cond ((equal? (peek stream) 'define)
         (next stream)
         (cons (article stream error) (articles stream error)))
        (else '())))

;<Article>  ::= define word <Body> end .
(define (article stream error)
  (let* ((w (next stream))
         (b (body stream error))
         (e (next stream)))
    
    (if (and (word? w) (equal? e 'end))
        (list w b)
        (error #f))))
  

;<Body> ::= if <Body> endif <Body> | integer <Body> | word <Body> | .
(define (body stream error)
  (cond ((equal? (peek stream) 'if)
         (let* ((i (next stream))
                (b (body stream error))
                (e (next stream)))           
           (if (equal? e 'endif)
               (cons (list i b) (body stream error))
               (error #f))))
        ((or (integer? (peek stream)) (word? (peek stream)))
         (cons (next stream) (body stream error)))
        (else '())))

(define (word? a)
  (and (symbol? a)
       (not (or (equal? a 'define) (equal? a 'if) (equal? a 'end) (equal? a 'endif)))))

(define the-tests-1
  (list (test (parse #(1 2 +)) '(() (1 2 +)))
        (test (parse #(x dup 0 swap if drop -1 endif)) '(() (x dup 0 swap (if (drop -1)))))
        (test (parse #( define -- 1 - end
          define =0? dup 0 = end
          define =1? dup 1 = end
          define factorial
              =0? if drop 1 exit endif
              =1? if drop 1 exit endif
              dup --
              factorial
              *
          end
          0 factorial
          1 factorial
          2 factorial
          3 factorial
          4 factorial )) ' (((-- (1 -))
   (=0? (dup 0 =))
   (=1? (dup 1 =))
   (factorial
    (=0? (if (drop 1 exit)) =1? (if (drop 1 exit)) dup -- factorial *)))
  (0 factorial 1 factorial 2 factorial 3 factorial 4 factorial)))
        (test (parse #(define word w1 w2 w3)) '#f)
        (test (parse #(define end end)) '#f)
        (test (parse #(define if end)) '#f)
        (test (parse #(define if  end endif)) '#f)
        (test (parse #()) '(()()))
        (test (parse #(+ + +)) '(()(+ + +)))
        (test (parse #(if if if if endif endif endif endif)) '(() ((if ((if ((if ((if ()))))))))))
        ;(test  ')
        ))
(run-tests the-tests-1)