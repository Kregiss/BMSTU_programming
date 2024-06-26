(define call/cc call-with-current-continuation)
(define env #f)

(define-syntax use-assertations
  (syntax-rules ()
    ((use-assertations)
     (call/cc (lambda (f) (set! env f))))))

(define-syntax assert
  (syntax-rules()
    ((assert expr?)
     (if (not expr?)
         (begin
           (display "FAILED: ")
           (write 'expr?)
           (newline)
           (env))))))

(use-assertations) ; Инициализация вашего каркаса перед использованием

; Определение процедуры, требующей верификации переданного ей значения:

(define (1/x x)
  (assert (not (zero? x))) ; Утверждение: x ДОЛЖЕН БЫТЬ ≠ 0
  (/ 1 x))

; Применение процедуры с утверждением:

(map 1/x '(1 2 3 4 5)) ; ВЕРНЕТ список значений в программу

(map 1/x '(-2 -1 0 1 2)) ; ВЫВЕДЕТ в консоль сообщение и завершит работу программы