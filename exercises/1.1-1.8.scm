;; Exercise 1.1
10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ; a = 3
(define b (+ a 1)) ; b = 4
(+ a b (* a b)) ; 19
(= a b) ; #f
(if (and (> b a) (< b (* a b))) 
      b
          a) ; 4
(cond ((= a 4) 6)
            ((= b 4) (+ 6 7 a))
                  (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
                  ((< a b) b)
                           (else -1))
      (+ a 1)) ; 16
      
      
;; Exercise 1.2      
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
      (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3
(define (take-2-sos a b c)
  (define (sos x y)
    (+ (* x x) (* y y)))
  (if (> a b)
    (if (> b c)
      (sos a b)
      (sos a c))
    (if (> a c)
      (sos a b)
      (sos b c))))

;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b)) ; a + |b|
(a-plus-abs-b 3 -3)

;; Exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
(test 0 (0))

;; Applicative-order: operator and operands is evaluated first 
;; (test 0 (0)) => (test 0 (0)) => (test 0 (0)) ...
;; Normal-order: operands would not be evaluated until it is needed
;; (test 0 (0)) => (if (= 0 0) 0 (0)) => 0
;; Indeed if we evaluate above expressions in Lisp, it will
;; yield error due to infinite loops

;; Exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

;; <consequent> part in 'cond' can be a sequence of expressions
;; but <consequent> in 'if' mus be a single expressions
;; In this particular case (sqrt-iter (improve guess x) x) will
;; be evaluated over and over again with the same <predicate> value
;; in the first loops. Says, the loop in this case is never stop.

;; Exercise 1.7
(define (square x)
  (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt (square 0.000000001)) ; 0.03125000000000001

;; Arithmetic operations are almost always performed with limited
;; precision. New 'sqrt' procedure below can handle this problem
;; Block structure included ;)

(define (new-sqrt x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x) 
        guess
      (sqrt-iter (improve guess x)
                 x)))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (sqrt x)
    (sqrt-iter 1.0 x))
  (define (good-enough? guess x) ; New function here!
    (< (/ (abs (- guess (improve guess x))) 
          guess) 0.001))
  (sqrt-iter 1.0 x))

(define (square x) (* x x))

(new-sqrt (square 1e-9)) ; 1.0003720056833836e-09

;; Exercises 1.8
;; Lexical scoping included ;)

(define (cr x) 
  (define (cr-iter guess)
    (if (good-enough? guess) 
        guess
        (cr-iter (improve guess))))
  (define (good-enough? guess)
    (< (/ (abs (- guess (improve guess))) 
          guess) 0.001))
  (define (improve guess)
    (/ (+ (/ x (square guess))  (* 2 guess))
       3))
  (define (square x) (* x x))
  (cr-iter 1.0))

(cr 27) ; expected: 3













