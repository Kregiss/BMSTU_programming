;92 символа
(load "unit-test.scm")

(define feature-if-else #t)
(define feature-while-loop #t)
(define feature-break-continue #t)

(define (vec-len xs)
  (length (vector->list xs)))

(define (flag action x y)
  (if (action x y) -1 0))

(define (search-ind xs value start)
  (and (< start (vec-len xs))
       (if (equal? (vector-ref xs start) value)
           start
           (search-ind xs value (+ 1 start )))))

(define (first-end_break xs start)
  (let ((a (search-ind xs 'wend start))
        (b (search-ind xs 'repeat start))
        (c (search-ind xs 'next start)))
    (cond
      ((and a b c) (if (< a b)
                       (if (< a c) a c)
                       (if (< b c) b c)))
      ((and a b) (if (< a b) a b))
      ((and a c) (if (< a c) a c))
      ((and b c) (if (< b c) b c))
      ((and a (not b) (not c)) a)
      ((and b (not a) (not c)) b)
      ((and c (not b) (not a)) c))))

(define (first-end_continue xs start)
  (let ((a (search-ind xs 'wend start))
        (b (search-ind xs 'until start))
        (c (search-ind xs 'next start)))
    (cond
      ((and a b c) (if (< a b)
                       (if (< a c) a c)
                       (if (< b c) b c)))
      ((and a b (not c)) (if (< a b) a b))
      ((and a c (not b)) (if (< a c) a c))
      ((and b c (not a)) (if (< b c) b c))
      ((and a (not b) (not c)) a)
      ((and b (not a) (not c)) b)
      ((and c (not b) (not a)) c))))

(define (interpret program stack)
  (let main_proc ((ind_word 0) (stack-in stack) (stack-out '()) (dict '()))
    (if (= ind_word (vec-len program)) ; т.е. дошли до конца program, просмотрели все индексы
        stack-in
        (let ((word (vector-ref program ind_word)))
          (cond
            ((number? word) (main_proc (+ 1 ind_word)
                                       (cons word stack-in)
                                       stack-out
                                       dict))  ; является ли числом
            ; встроенные слова
            ; Арифметические операции
            ((equal? word '+)   (main_proc (+ 1 ind_word)
                                           (cons (+ (car stack-in)
                                                    (cadr stack-in))
                                                 (cddr stack-in))
                                           stack-out dict))
            ((equal? word '-)   (main_proc (+ 1 ind_word)
                                           (cons (- (cadr stack-in)
                                                    (car stack-in))
                                                 (cddr stack-in))
                                           stack-out
                                           dict))
            ((equal? word '*)   (main_proc (+ 1 ind_word)
                                           (cons (* (car stack-in)
                                                    (cadr stack-in))
                                                 (cddr stack-in))
                                           stack-out
                                           dict))
            ((equal? word '/)   (main_proc (+ 1 ind_word)
                                           (cons (quotient (cadr stack-in)
                                                           (car stack-in))
                                                 (cddr stack-in))
                                           stack-out
                                           dict))
            ((equal? word 'mod) (main_proc (+ 1 ind_word)
                                           (cons (remainder
                                                  (cadr stack-in)
                                                  (car stack-in))
                                                 (cddr stack-in))
                                           stack-out
                                           dict))
            ((equal? word 'neg) (main_proc (+ 1 ind_word)
                                           (cons (* -1
                                                    (car stack-in))
                                                 (cdr stack-in))
                                           stack-out
                                           dict))
            ; Операции сравнения
            ((equal? word '=) (main_proc (+ 1 ind_word)
                                         (cons (flag = (car stack-in)
                                                     (cadr stack-in))
                                               (cddr stack-in))
                                         stack-out
                                         dict))
            ((equal? word '>) (main_proc (+ 1 ind_word)
                                         (cons (flag > (cadr stack-in)
                                                     (car stack-in))
                                               (cddr stack-in))
                                         stack-out
                                         dict))
            ((equal? word '<) (main_proc (+ 1 ind_word)
                                         (cons (flag < (cadr stack-in)
                                                     (car stack-in))
                                               (cddr stack-in))
                                         stack-out
                                         dict))
            ; Логические операции
            ((equal? word 'not) (main_proc (+ 1 ind_word)
                                           (cons (if (= 0 (car stack-in)) -1 0)
                                                 (cdr stack-in))
                                           stack-out
                                           dict))
            ((equal? word 'and) (main_proc (+ 1 ind_word)
                                           (cons (if (or (= 0 (car stack-in))
                                                         (= 0 (cadr stack-in))) 0 -1)
                                                 (cddr stack-in))
                                           stack-out
                                           dict))
            ((equal? word 'or)  (main_proc (+ 1 ind_word)
                                           (cons (if (and (= 0 (car stack-in))
                                                          (= 0 (cadr stack-in))) 0 -1)
                                                 (cddr stack-in))
                                           stack-out
                                           dict))
            ; Операции со стеком
            ((equal? word 'drop)  (main_proc (+ 1 ind_word)
                                             (cdr stack-in)
                                             stack-out
                                             dict))
            ((equal? word 'swap)  (main_proc (+ 1 ind_word)
                                             (cons (cadr stack-in)
                                                   (cons (car stack-in)
                                                         (cddr stack-in)))
                                             stack-out
                                             dict))
            ((equal? word 'dup)   (main_proc (+ 1 ind_word)
                                             (cons (car stack-in) stack-in)
                                             stack-out
                                             dict))
            ((equal? word 'over)  (main_proc (+ 1 ind_word)
                                             (cons (cadr stack-in) stack-in)
                                             stack-out
                                             dict))
            ((equal? word 'rot)   (main_proc (+ 1 ind_word)
                                             (cons (caddr stack-in)
                                                   (cons (cadr stack-in)
                                                         (cons (car stack-in)
                                                               (cdddr stack-in))))
                                             stack-out
                                             dict))
            ((equal? word 'depth) (main_proc (+ 1 ind_word)
                                             (cons (length stack-in) stack-in)
                                             stack-out
                                             dict))
            ; Управляющие конструкции
            ((equal? word 'define) (main_proc (+ 1 (search-ind program 'end ind_word))
                                              stack-in
                                              stack-out
                                              (cons (list
                                                     (vector-ref program (+ 1 ind_word))
                                                     (+ 2 ind_word)) dict))) ; запись названия и того, что делает новая процедура в словарь
            ((equal? word 'end)    (main_proc (car stack-out)
                                              stack-in
                                              (cdr stack-out)
                                              dict))
            ((equal? word 'exit)   (main_proc (car stack-out)
                                              stack-in
                                              (cdr stack-out)
                                              dict))
            ;(define feature-if-else #t)
            ((equal? word 'if)  (main_proc (if (= 0 (car stack-in))
                                               (+ 1 (search-ind program 'endif ind_word))
                                               (+ 1 ind_word))
                                           (cdr stack-in)
                                           stack-out
                                           dict))
            ((equal? word 'endif)  (main_proc (+ 1 ind_word) stack-in stack-out dict))
            ((equal? word 'if)  (if (search-ind program 'else ind_word)
                                    (main_proc (if (= 0 (car stack-in))
                                                   (+ 1
                                                      (search-ind program 'else ind_word))
                                                   (+ 1 ind_word))
                                               (cdr stack-in)
                                               stack-out
                                                dict)
                                    (main_proc (if (= 0 (car stack-in))
                                                   (+ 1
                                                      (search-ind program 'endif ind_word))
                                                   (+ 1 ind_word))
                                               (cdr stack-in)
                                               stack-out
                                               dict)))
            ((equal? word 'else)   (if (= 0 (car stack-in))
                                       (main_proc (+ 1 ind_word)
                                                  (cdr stack-in)
                                                  stack-out
                                                  dict)
                                       (main_proc (+ 1
                                                     (search-ind program 'endif ind_word))
                                                  stack-in
                                                  stack-out
                                                  dict)))
            ((equal? word 'endif)  (main_proc (+ 1 ind_word) stack-in stack-out dict))
            ;(define feature-while-loop #t)
            ((equal? word 'while)  (if (= 0 (car stack-in))
                                       (main_proc (+ 1 (search-ind program 'wend ind_word))
                                                  (cdr stack-in)
                                                  stack-out
                                                  dict)
                                       (main_proc (+ 1 ind_word)
                                                  (cdr stack-in)
                                                  (cons ind_word stack-out)
                                                  dict)))
            ((equal? word 'wend)   (main_proc (car stack-out)
                                              stack-in
                                              (cdr stack-out)
                                              dict))
            ;(define feature-break-continue #t)
            ((equal? word 'break)    (main_proc (+ 1 (first-end_break program ind_word))
                                                stack-in
                                                stack-out
                                                dict))
            ((equal? word 'continue) (main_proc (first-end_continue program ind_word)
                                                stack-in
                                                stack-out
                                                dict))

            (else (main_proc (cadr (assoc word dict))
                             stack-in
                             (cons (+ 1 ind_word) stack-out)
                             dict))
            )))))

(define the-tests
  (list (test (interpret #(1 if 100 else 200 endif) '())
              '(100))

        (test (interpret #(define =0? dup 0 = end
                           5 repeat 1 swap 1 - =0? until drop)
                         '())
              '(1 1 1 1 1))

        (test (interpret #(while wend) '(3 7 4 0 5 9))
              '(5 9))

        (test (interpret #(define sum
               dup
               while + swap dup wend
               drop
             end
             1 2 3 0 4 5 6 sum)
           '())
              '(15 3 2 1))

        (test (interpret #(2 3 * 4 5 * +) '())
              '(26))
        
        (test (interpret #(   define -- 1 - end
                5 -- --      ) '())
              '(3))
        
        (test (interpret #(   define abs
                    dup 0 <
                    if neg endif
                end
                 9 abs
                -9 abs      ) (quote ()))
              '(9 9))
        
        (test (interpret #(   define =0? dup 0 = end
                define <0? dup 0 < end
                define signum
                    =0? if exit endif
                    <0? if drop -1 exit endif
                    drop
                    1
                end
                 0 signum
                -5 signum
                10 signum       ) (quote ()))
              '(1 -1 0))
        
        (test (interpret #(   define -- 1 - end
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
                4 factorial     ) (quote ()))
              '(24 6 2 1 1))
        
        (test (interpret #(   define =0? dup 0 = end
                define =1? dup 1 = end
                define -- 1 - end
                define fib
                    =0? if drop 0 exit endif
                    =1? if drop 1 exit endif
                    -- dup
                    -- fib
                    swap fib
                    +
                end
                define make-fib
                    dup 0 < if drop exit endif
                    dup fib
                    swap --
                    make-fib
                end
                10 make-fib     ) (quote ()))
              '(0 1 1 2 3 5 8 13 21 34 55))
        
        (test (interpret #(   define =0? dup 0 = end
                define gcd
                    =0? if drop exit endif
                    swap over mod
                    gcd
                end
                90 99 gcd
                234 8100 gcd    ) '())
              '(18 9))))

(run-tests the-tests)
