;; Load 'mceval' module from book code
(load "../resources/book-code/ch4-mceval.scm")
;; Load unit testing module from racket for testing new primitive procedures
(require rackunit)


;; ========================================================================== 
;; EXERCISE 4.1
;; ========================================================================== 

;; Book version
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(list-of-values '((displayln 1)
                  (displayln 2)
                  (displayln 3)) (current-namespace))


