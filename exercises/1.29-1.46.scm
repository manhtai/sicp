;; EXERCISE 1.29

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;; Simpson's Rule

(define (simpson f a b n)
  (define (add-h x) (+ x (* 2 (/ (- b a) n))))
  (* (/ (/ (- b a) n)
        3)
     (+ (- 0 (f a) (f b))
        (* (sum f (+ a (/ (- b a) n)) add-h b) 4)
        (* (sum f a add-h b) 2))))

(simpson cube 0 1 100) ; 1/4 or so, I don't know :D
(simpson cube 0 1 1000) ; 1/4

;; EXERCISE 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; EXERCISE 1.31
;; a) iterative product

(define (product term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (define (add-one x) (+ x 1))
  (define (identity x) x)
  (product identity 1 add-one n))

(factorial 5) ; 120

(define (prod-pi n)
  (define (add-2 x) (+ x 2))
  (define (frac+1 a) (/ a (+ a 1)))
  (define (frac-1 a) (/ a (- a 1)))
  (* 4.0
     (product frac+1 2 add-2 n)
     (product frac-1 4 add-2 n)))

(prod-pi 1000)

;; b) recursive product

(define (product term a next b)
  (if (< b a)
      1
      (* (term a)
         (product term (next a) next b))))

(factorial 5) ; The same!

;; EXERCISE 1.32
;; More general than product and sum!

(define (accumulate combiner null-value term a next b)
  (if (< b a)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(factorial 5) ; 120?

;; Iterative version

(define (accumulate combiner null-value term a next b)
  (define (acc-iter a result)
    (if (< b a)
        result
        (acc-iter (next a) (combiner (term a) result))))
  (acc-iter a null-value))

(factorial 5) ; 

;; EXERCISE 1.33

(define (filtered-accumulate combiner null-value term a next b condition)
  (define (acc-iter a result)
    (if (< b a)
        result
        (acc-iter (next a) (combiner (if (condition a)
                                         (term a)
                                         null-value)
                                     result))))
  (acc-iter a null-value))

;; a)

(define (sos-of-prime a b)
  (define (add-one x) (+ x 1))
  (filtered-accumulate + 0 square a add-one b prime?))

;; b)

(define (part-b n)
  (define (add-one x) (+ x 1))
  (define (identity x) x)
  (define (gdc? x) (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 add-one n gdc?))

(part-b 3) ; 2 

;; EXERCISE 1.34
(define (square x) (* x x))

(define (f g) (g 2))
(f square) ; 4
(f (lambda (z) (* z (+ z 1)))) ; 6

;; (f f) => (f 2) => (2 2) error

;; EXERCISE 1.35

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ (/ 1 x) 1)) 1.0)

;; EXERCISE 1.36

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 10.0)

;; Compare with average-damp
(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(fixed-point (average-damp (lambda (x) (/ (log 1000) (log x)))) 10.0)

;; Fewer steps than not using average-damp

;; ============================================================================
;; EXERCISE 1.37
;; Infinite continued fraction

;; Recusive process
(define (cont-frac n d k)
  (define (frac i)
    (let ((ni (n i))
          (di (d i)))
         (if (= i 1)
             (/ ni di)
             (/ ni (+ di (frac (- i 1)))))))
  (frac k))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

(define (get-k accurate)
  (define golden 0.61803398874989484820458683436565)
  (define (good? phi)
    (< (abs (- phi golden)) accurate))
  (define (approx guess k)
    (if (good? guess)
        (display k)
        (approx (cont-frac (lambda (i) 1.0)
                           (lambda (i) 1.0)
                           k) (+ k 1))))
  (approx 0 1))

(get-k 0.0001)

;; Iterative process
(define (cont-frac n d k)
  (define (frac i result)
    (let ((ni (n i))
          (di (d i)))
         (if (= i 0)
             result
             (frac (- i 1)
                   (/ ni (+ di result))))))
  (frac k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

;; EXERCISE 1.38

(+ 2 (cont-frac (lambda (i) 1.0)
                (lambda (i)
                        (let ((r (remainder i 3)))
                             (cond ((= r 1) 1)
                                   ((= r 0) 1)
                                   (else (* 2 (/ (+ i 1) 3))))))
                100))

;; EXERCISE 1.39

(define (tan-cf x k)
  (/ (cont-frac (lambda (i) (- (* x x)))
                (lambda (i) (- (* 2 i) 1))
                k)
     (- x)))

(tan-cf pi 100) ; ~ 0

;; ============================================================================
;; EXERCISE 1.40

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; cubic for solving x^3 + ax^2 + bx + c

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

(newtons-method (cubic -1 1 -6) 1) ; 2

;; EXERCISE 1.41
(define (double g)
  (lambda (x) (g (g x))))

(((double (double double)) inc) 5) ; apply (2^2)^2 time inc

;; EXERCISE 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6) ; 49

;; EXERCISE 1.43
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) ((compose f (repeated f (- n 1))) x))))

((repeated square 2) 5) ; 625

;; EXERCISE 1.44
(define (smooth f)
  (lambda (x) (average
                (f (- x dx))
                (f (x))
                (f (+ x dx)))))

(define (n-smooth f n)
  (repeated smooth n) f)

;; EXERCISE 1.45
;; I have 3 things:
;; 1, fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; 2, average-damp
(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

;; 3, repeated
(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) ((compose f (repeated f (- n 1))) x))))

;; I must do 2 things:
;; 1, determine how many average-damp need to compute n-root
;; I don't know, I guess
(define (how n)
  (floor (/ (log n) (log 2))))
;; 2, write a procedure to computing n-th root
;; I know, I write
(define (n-root x n)
  (fixed-point ((repeated average-damp (how n))
                (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(n-root 100 2) ; 10
(n-root 1000000 6) ; 10

;; EXERCISE 1.46
(define (iterative-improve good-enough improve)
  (lambda (guess) (if (good-enough guess)
                      guess
                      ((iterative-improve good-enough improve) (improve guess)))))

;; iterative-improve procedure take 2 procedures and return a procedure that have
;; an argument is guess, hence the recursive thing here is to call iterative-improve
;; with new guess improved by improve procedure

;; Rewrite sqrt
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (sqrt x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess) (average guess (/ x guess)))) 1.0))
(sqrt 9) ; 3


;; Rewrite fixed-point
(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) 0.0000001))
                      (lambda (guess) (f guess))) first-guess))

(fixed-point (lambda (x) (+ (/ 1 x) 1)) 1.0) ; phi = 1.618...












