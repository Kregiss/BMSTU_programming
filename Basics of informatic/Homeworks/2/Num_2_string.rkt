; O(len(str)^2)
(define (string-trim-left str)
  (if (or (equal? (car (string->list str)) #\space)
          (equal? (car (string->list str)) #\tab)
          (equal? (car (string->list str)) #\newline))
      (string-trim-left (list->string (cdr (string->list str))))
      str))
;(string-trim-left  "\t\tabc def")   ;=> "abc def"

; O(len(str)^2)
(define (string-trim-right str)
  (if (or (equal? (car (reverse (string->list str))) #\space)
          (equal? (car (reverse (string->list str))) #\tab)
          (equal? (car (reverse (string->list str))) #\newline))
      (string-trim-right (list->string (reverse (cdr (reverse (string->list str))))))
      str))
;(string-trim-right "abc def\t")     ;=> "abc def"

;O(len(str)^2)
(define (string-trim str)
  (string-trim-left (string-trim-right str)))
;(string-trim       "\t abc def \n") ;=> "abc def"

;O(len(a) ^ 2)
(define (string-prefix? a b)
  (if (not (null? (string->list a)))
      (and (<= (length (string->list a)) (length (string->list b)))
          (and (equal? (car (string->list a)) (car (string->list b)))
               (string-prefix? (list->string (cdr (string->list a)))
                               (list->string (cdr (string->list b))))))
      (null? (string->list a))))
;(string-prefix? "abc" "abcdef")  ;=> #t
;(string-prefix? "bcd" "abcdef")  ;=> #f
;(string-prefix? "abcdef" "abc")  ;=> #f

;O(len(a) ^ 2)
(define (string-suffix? a b)
  (if (not (null? (string->list a)))
      (and (<= (length (string->list a)) (length (string->list b)))
          (and (equal? (car (reverse (string->list a))) (car (reverse (string->list b))))
               (string-prefix? (list->string (cdr (reverse (string->list a))))
                               (list->string (cdr (reverse (string->list b)))))))
      (null? (string->list a))))
;(string-suffix? "def" "abcdef")  ;=> #t
;(string-suffix? "bcd" "abcdef")  ;=> #f

;O(len(a) * len(a) * len(b))
(define (string-infix? a b)
  (or (equal? a b)
      (and (not (null? (string->list b)))
           (or (string-prefix? a b)
               (string-suffix? a b)
               (string-infix? a (list->string (cdr (string->list b))))))))
;(string-infix? "def" "abcdefgh") ;=> #t
;(string-infix? "abc" "abcdefgh") ;=> #t
;(string-infix? "fgh" "abcdefgh") ;=> #t
;(string-infix? "ijk" "abcdefgh") ;=> #f
;(string-infix? "bcd" "abc")      ;=> #f
;(string-infix? "" "")            ;=> #t

(define (delete-str str sep)
  (if (= 0 (length str))
      str
      (if (< (length sep) (length str))
          (if (equal? sep (reverse (list-tail (reverse str) (- (length str) (length sep)))))
              (delete-str (list-tail str (length sep)) sep)
              (cons (reverse (list-tail (reverse str) (- (length str) 1)))
                    (delete-str (cdr str) sep)))
          (list str))))

;O(len(sep) * len(sep) * len(str))
(define (string-split str sep)
  (if (string-infix? sep str)
      (map list->string (delete-str (string->list str) (string->list sep)))
      (list str)))
;(string-split "x;y;z" ";")       ;=> ("x" "y" "z")
;(string-split "x-->y-->z" "-->") ;=> ("x" "y" "z")
