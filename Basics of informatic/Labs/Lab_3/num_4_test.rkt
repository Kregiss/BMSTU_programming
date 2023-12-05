(define-syntax factorize
  (syntax-rules (expt)
    ((factorize '(- (expt x 2) (expt y 2))) '(* (- x y) (+ x y)))
    ((factorize '(- (expt x 3) (expt y 3))) '(* (- x y) (+ (expt x 2) (* x y) (expt y 2))))
    ((factorize '(+ (expt x 3) (expt y 3))) '(* (+ x y) (+ (expt x 2) (- (* x y)) (expt y 2))))))


(define (factorize ls)
  (let ((a (cadr (cadr ls)))
        (b (cadr (caddr ls))))
    (if (equal? (caddr (cadr ls)) 2)
        (list '* (list '- a b) (list '+ a b))
        (if (equal? (car ls) '-)
            (list '* (list '- a b) (list '+ (list 'expt a '2) (list '* a b) (list 'expt b '2)))
            (list '* (list '+ a b) (list '+ (list 'expt a '2) (list '- (list '* a b)) (list 'expt b '2)))))))

;(factorize '(- (expt x 2) (expt y 2)))   => (* (- x y) (+ x y))

;(factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))
;  => (* (- (+ first 1) (- second 1)) (+ (+ first 1) (- second 1)))
;(load "unit-test.scm")
;(define the-tests
;  (list (test (factorize '(- (expt x 2) (expt y 2))) (* (- x y) (+ x y)))
;        (test (factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))  (* (- (+ first 1) (- second 1)) (+ (+ first 1) (- second 1))))))

;(run-tests the-tests)

(eval (list (list 'lambda 
                      '(x y) 
                      (factorize '(- (expt x 2) (expt y 2))))
                1 2)
          (interaction-environment))