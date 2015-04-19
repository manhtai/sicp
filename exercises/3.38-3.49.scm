;; EXERCISE 3.38
;: 1: (set! balance (+ balance 10))
;: 2: (set! balance (- balance 20))
;: 3: (set! balance (- balance (/ balance 2)))

;; 1,2 = 2,1, so we have
;; 1,2,3
;; 1,3,2
;; 2,3,1
;; 3,1,2

;; ========================================================================== 
;; I leave it here to later
;; ========================================================================== 

(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))
x

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))
x



