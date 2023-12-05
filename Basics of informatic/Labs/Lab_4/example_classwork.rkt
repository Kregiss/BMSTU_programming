; Пример к заданию 2

(save-data '(1 2 3 4) "1234.txt")

(apply * (load-data "1234.txt"))  ; 24

;  Пример к заданию 4

(my-if #t #f 'a)  ; #f

; К заданию 5 - ачивка: решить без эллипсисов(+1 балл)

(define (f1 x y)
  (my-let ((y x)
           (x y))
          (+ (* 100 x) y)))

(f1 2 3)  ; 302

; подсказка

(define-syntax rev
  (syntax-rules ()
    ((rev-expr) (do-rev expr ()))))

(define-syntax do-rev
  (syntax-rules ()
    ((do-rev (hd . tl) res)
     (do-rev tl (hd . res)))
    ((do-rev () res) res)))

; (rev ("Hello, world!\n" display))  => Hello, world!
; (rev (3 10 -))                     => 7


; Ачивка 2 - написать рекурсивный макрос rev-rec:
; (rev-rec ((3 10 -) (5 10 /) *))) - 14

; Пример к 6 заданию

(let ((a 2) (b 3))
  (cout << "a + b = " << ( + a b) << endl))
; a + b = 5  