;; Here I tweak (display-stream stream n) for displaying just for n line,
;; default n = 17, you can omit it to use the default or set it to any value you want

;; ========================================================================== 
;; EXERCISE 3.50
;; ========================================================================== 
(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(stream-map + (cons-stream 1 (cons-stream 2 3)))

;; ========================================================================== 
;; EXERCISE 3.51
;; ========================================================================== 

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
;; 0
;; because 'stream-map' map 'show' to the first element of stream, i.e, 0

(stream-ref x 5)
;; 1 2 3 4 55
;; because 0 is calculated before, now it calculate 1, 2, 3, 4, 5 and show
;; number in that order, the last 5 is the result of this procedure, so double 5

(stream-ref x 7)
;; 6 77
;; because 0-5 are calculated, now it calculate 6, 7 and display thoses
;; the last 7 is the result of this procedure, so double 7


;; ========================================================================== 
;; EXERCISE 3.52
;; ========================================================================== 

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
sum ;; 1
;; Obvious?
;; seq = 1+

(define y (stream-filter even? seq))
sum ;; 6
;; Next even number
;; i | sum | seq
;; 2 | 3   | 3+ (2 20)
;; 3 | 6   | 6+ (3 20) => 6 is even

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
sum ;; 10
;; Next 5-divider
;; i | sum | seq
;; 4 | 10  | 10+ (4 20) => 10 is % 5 = 0

(stream-ref y 7)
sum ;; 136
;; Next 7 even numbers (from 4 because to 3 were calculated before)
;; i  | sum  | seq
;; 4  | 10   | 10+   (4 20)   even
;; 5  |      | 15    (5  20)
;; 6  |      | 21    (6  20)
;; 7  |      | 28    (7  20)  even
;; 8  |      | 36    (8  20)  even
;; 9  |      | 45    (9  20)
;; 10 |      | 55    (10 20)
;; 11 |      | 66    (11 20)  even
;; 12 |      | 78    (12 20)  even
;; 13 |      | 91    (13 20)
;; 14 |      | 105   (14 20)
;; 15 |      | 120   (15 20) even
;; 16 | 136  | 136+  (16 20) even

(display-stream z 17)
sum ;; 210
;; Print all z from i = 5 (4 was calculated before)
;; i  | sum  | seq
;; 5  |      | 15    (5  20)
;; 6  |      | 21    (6  20)
;; 7  |      | 28    (7  20)
;; 8  |      | 36    (8  20)
;; 9  |      | 45    (9  20)
;; 10 |      | 55    (10 20)
;; 11 |      | 66    (11 20)
;; 12 |      | 78    (12 20)
;; 13 |      | 91    (13 20)
;; 14 |      | 105   (14 20)
;; 15 |      | 120   (15 20)
;; 16 | 136  | 136+  (16 20)
;; 17 |      | 153   (17 20)
;; 18 |      | 171   (18 20)
;; 19 |      | 190   (19 20)
;; 20 | 210  | 210+  (20 20)


;; ========================================================================== 
;; EXERCISE 3.53
;; ========================================================================== 
(define s (cons-stream 1 (add-streams s s)))
;; stream(n) = stream(n-1) + stream(n-1) = 2*stream(n-1)
;; 1 2 4 8 16 ...
(display-stream s 17)

;; ========================================================================== 
;; EXERCISE 3.54
;; ========================================================================== 

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))
;; factorials(n) = factorials(n-1) * n

(display-stream factorials 17)

;; ========================================================================== 
;; EXERCISE 3.55
;; ========================================================================== 
;; partial-sum(n) = partial-sum(n - 1) + stream(n)

(define (partial-sums s)
  (cons-stream 
    (stream-car s)          ;| cons this to stream(n-1) to make stream(n)
    (add-streams 
      (stream-cdr s)        ;| stream(n-1)
      (partial-sums s))))   ;| partial-sum(n-1)

(define ps (partial-sums integers))

(display-stream ps 17)

;; ========================================================================== 
;; EXERCISE 3.56
;; ========================================================================== 

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) 
                                (merge (scale-stream S 3) 
                                       (scale-stream S 5)) )))

(display-stream S 17)

;; ========================================================================== 
;; EXERCISE 3.57
;; ========================================================================== 

;; When 'delay' is cached, we need (n-1) addition to calculate fibs(n)
;; when it is not, we need fibs(n) - 1 

;; ========================================================================== 
;; EXERCISE 3.58
;; ========================================================================== 

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(expand 1 7 10)
;; 1 4 2 8 5 7 4 2 8 5 7 ...
(expand 3 8 10)
;; 3 7 5 0 0 0 ...
;; What is the point of this exercise?

;; ========================================================================== 
;; EXERCISE 3.59
;; ========================================================================== 

;; a)
(define (integrate-series s)
  (stream-map / s integers))

(display-stream (integrate-series ones) 17)

;; b)
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series 
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series 
  (cons-stream 0 (integrate-series cosine-series)))

;; ========================================================================== 
;; EXERCISE 3.60
;; ========================================================================== 

;;   a0, a1, ... an
;; * b0, b1, ... bn
;; = a0 * (b0 .. bn) + (a1 ... an) * (b0 ... bn)
;; = a0 * b0 + a0 * (b1 ... bn) + (a1 ... an) * (b0 ... bn)

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

;; This doesn't really work because mul-series can't print longer than its inputs
(display-stream (mul-series (stream-enumerate-interval 1 3)
                            (stream-enumerate-interval 1 3))
                17)

;; ========================================================================== 
;; EXERCISE 3.61
;; ========================================================================== 

(define (invert-unit-series s)
  (cons-stream
    1
    (map - (mul-series (stream-cdr s)
                       (invert-unit-series s)))))

;; ========================================================================== 
;; EXERCISE 3.62
;; ========================================================================== 

;; (a0 a1 a2 ... an) / (b0 b1 b2 ... bn)
;; = A * (1/B)
;; = A * (1/b0) * (1/S) in which S has constant term equal 1
;; S = 1/b0 * B
;; so we have A * (1/b0) * (1 / (B*1/b0))

(define (div-series s1 s2)
  (let ((c (stream-car s2)))
    (if (= c 0)
        (error "Denominator must not have a zero constant term")
        (scale-stream (mul-series s1 
                                  (invert-unit-series (scale-stream s2 (/ 1 c))))
                      (/ 1 c)))))

(define tangent-series
  (div-series sine-series cosine-series))

;; tan^2 + 1 = 1 / cos^2
(define tan-square
  (mul-series tangent-series tangent-series))

;; ========================================================================== 
;; EXERCISE 3.63
;; ========================================================================== 

;; Original version
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;; Louis version
(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

;; In the original version 'guesses' was used to cached results, so it is better.
;; If cache was not implemented, then nothing is different


;; ========================================================================== 
;; EXERCISE 3.64
;; ========================================================================== 

(define (stream-limit s n)
  (let ((next-promise (stream-cdr s)))
    (if (< (abs (- (stream-car s) (stream-car next-promise))) n)
        (stream-car next-promise)
        (stream-limit next-promise n))))
;; we can use (stream-ref) instead of (stream-cdr)


(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.00001)


;; ========================================================================== 
;; EXERCISE 3.65
;; ========================================================================== 

;; Stream 1
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2
  (partial-sums (ln2-summands 1)))

(display-stream ln2 7)

;; Stream 2
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))    
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(display-stream (euler-transform ln2) 7)

;; Stream 3
(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(display-stream (accelerated-sequence euler-transform
                                      ln2)
                7)


;; ========================================================================== 
;; EXERCISE 3.66
;; ========================================================================== 
;; Infinite streams of pairs

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define p (pairs integers integers))

;; Examine it a bit
(display-stream p 7)

;; CHALLENGE:
;; f(i,j) = 2^i - 2, i = j
;; f(i,j) = 2^i * (j-i) + 2^(i-1) - 2, i < j

;; ========================================================================== 
;; EXERCISE 3.67
;; ========================================================================== 

(define (pairs-2 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
      (interleave
        (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t)) 
        (stream-map (lambda (x) (list x (stream-car t)))
                    (stream-cdr s)))
      (pairs-2 (stream-cdr s) (stream-cdr t)))))

(define p-2 (pairs-2 integers integers))

(display-stream p-2 7)

;; ========================================================================== 
;; EXERCISE 3.68
;; ========================================================================== 
;; Louis' version

(define (pairs-l s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs-l (stream-cdr s) (stream-cdr t))))

;; No 'cons-stream' will cause infinite loop

;; ========================================================================== 
;; EXERCISE 3.69
;; ==========================================================================

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (p) (list (stream-car s) (car p) (cadr p)))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s)
             (stream-cdr t)
             (stream-cdr u)))))

(define trip-int (triples integers integers integers))

(display-stream trip-int 7)

(define (pythago? t)
  (let ((a (car t))
        (b (cadr t))
        (c (caddr t)))
    (= (square c) (+ (square a)
                      (square b)))))
(define pythagorean
  (stream-filter pythago? trip-int))

(display-stream pythagorean 7)

;; ========================================================================== 
;; EXERCISE 3.70
;; ==========================================================================

;; Merge 2 streams of pairs in (w) order
(define (merge-weighted s1 s2 w)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2))
               (w1 (w (stream-car s1)))
               (w2 (w (stream-car s2))))
           (cond ((< w1 w2)
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 w)))
                 ((> w1 w2)
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) w)))
                 (else
                  (cons-stream s1car
                               (merge-weighted (stream-cdr s1)
                                               s2
                                               w))))))))

;; Make weighted pairs
(define (weighted-pairs s t w)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) w)
    w)))

;; a)
(define pa (weighted-pairs integers integers (lambda (p) (+ (car p) (cadr p)))))

(display-stream pa 7)

;; b)
(define pt (weighted-pairs integers integers (lambda (p) (+ (* 2 (car p))
                                                            (* 3 (cadr p))
                                                            (* 5 (car p) (cadr p))))))
(define (not235? p)
  (define (n235? x)
    (not (or (= 0 (remainder x 2))
             (= 0 (remainder x 3))
             (= 0 (remainder x 5)))))
  (and (n235? (car p))
       (n235? (cadr p))))

(define pb
  (stream-filter not235? pt))

(display-stream pb 7)

;; ========================================================================== 
;; EXERCISE 3.71
;; ========================================================================== 

;; Ramanujan numbers
(define (c p)
  (+ (cube (car p))
     (cube (cadr p))))

(define pc (weighted-pairs integers integers c))

(define (pR p n)
    (let ((s0 (stream-ref p 0))
          (s1 (stream-ref p 1)))
        (cond ((and (= (c s0) (c s1))
                     (> n 0))
              (display (c s1))
              (newline)
              (pR (stream-cdr (stream-cdr p)) (- n 1)))
              ((> n 0) (pR (stream-cdr p) n)))))

(pR pc 6)

;; Stream?

(define (pRS p)
  (let ((c0 (c (stream-ref p 0)))
        (c1 (c (stream-ref p 1))))
      (cond ((= c0 c1)
             (cons-stream
               c1
               (pRS (stream-cdr (stream-cdr p)))))
            (else
              (pRS (stream-cdr p))))))

(display-stream (pRS pc) 7)


;; ========================================================================== 
;; EXERCISE 3.72
;; ========================================================================== 

(define (ss p)
  (+ (square (car p))
     (square (cadr p))))

(define ps (weighted-pairs integers integers ss))

(define (p72 p)
  (let ((s0 (ss (stream-ref p 0)))
        (s1 (ss (stream-ref p 1)))
        (s2 (ss (stream-ref p 2))))
      (cond ((= s0 s1 s2)
             (crns-stream
               s2
               (p72 (stream-cdr (stream-cdr (stream-cdr p))))))
            (else
              (p72 (stream-cdr p))))))

(display-stream (p72 ps) 7)

;; ========================================================================== 
;; EXERCISE 3.73
;; ========================================================================== 

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams
      (scale-stream i R)
      (integral (scale-stream i (/ 1 C))
                v0
                dt))))

(define RC1 (RC 5 1 0.5))

(display-stream (RC1 ones 1) 7)

;; ========================================================================== 
;; EXERCISE 3.74
;; ========================================================================== 

;; Alyssa's version
(define (sign-change-detector a b)
  (cond ((and (< a 0) (> b 0)) -1)
        ((and (> a 0) (< b 0)) 1)
        (else 0)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define sense-data
  (list->stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define zero-crossings (make-zero-crossings sense-data 0))

(display-stream zero-crossings 11)

;; Eva's version
(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

(display-stream zero-crossings 11)


;; ========================================================================== 
;; EXERCISE 3.75
;; ========================================================================== 

(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt))))

;; Bug: Must have 'last-avpt' to compare with 'avpt'

;; Fix:
(define (make-zero-crossings input-stream last-avpt last-value)
  (let ((avpt (average (stream-car input-stream) last-value)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt
                                      (stream-car input-stream)))))

(define zero-crossings (make-zero-crossings sense-data 0 0))

(display-stream zero-crossings 11)


;; ========================================================================== 
;; EXERCISE 3.76
;; ========================================================================== 

(define (smooth input-stream last-value)
  (let ((avpt (average (stream-car input-stream) last-value)))
    (cons-stream avpt
                 (smooth (stream-cdr input-stream)
                         (stream-car input-stream)))))

(define smooth-data (smooth sense-data 0))
(display-stream smooth-data 11)

(define zero-crossings
  (stream-map sign-change-detector
              smooth-data
              (cons-stream 0 smooth-data)))

(display-stream zero-crossings 11)

;; ========================================================================== 
;; EXERCISE 3.77
;; ========================================================================== 

;; Delayed argument
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; Find e
(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

;; Fixed procedure
(define (integral integrand initial-value dt)
  (cons-stream initial-value
     (let ((inte (force integrand)))                    ; Force here
          (if (stream-null? inte)
              the-empty-stream
              (integral (delay (stream-cdr inte))       ; Then delay here
                        (+ (* dt (stream-car inte))
                        initial-value)
                        dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

;; ========================================================================== 
;; Differential equation, I don't know it yet, so I'll come back here later.
;; Promised.
;; EXERCISE 3.78
;; EXERCISE 3.79
;; EXERCISE 3.80
;; ========================================================================== 

;; ========================================================================== 
;; EXERCISE 3.81
;; ========================================================================== 
;; support rand-update
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

;; Old rand generator
(define rand
  (let ((x 7))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (begin
               (set! x (rand-update x))
               x))
            ((eq? m 'reset)
             (lambda (n) (set! x n)))))
    dispatch))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 7)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

;; New rand generator!
(define random-numbers
  (cons-stream 7
              (stream-map rand-update random-numbers)))
(display-stream random-numbers 5)

;; My solution
(define (rand-generate random-numbers request)
  (define (rand-n rn re n)
    (let ((r (stream-car re)))
      (cons-stream
        (stream-ref random-numbers (if (= r 1) (+ n 1) 0))
        (rand-n (if (= r 1) (stream-cdr rn) random-numbers)
                (stream-cdr re)
                (if (= r 1) (+ n 1) 0)))))
  (rand-n random-numbers request 0))

(define request (list->stream '(1 1 1 1 1 0 1 1 1 1 1))) ; 1 for generate, 0 for reset
(display-stream (rand-generate random-numbers request) 10)

;; Another's solution, clearer and unstandable
(define (random-generate commands)
  (define random-init 7)
  (define (update-on-command com number)
    (cond ((= com 0) random-init)
          ((= com 1) (rand-update number))
          (else (error "unknown command: RANDOM-GENERATOR-STREAM"))))

  (define random-stream
    (cons-stream random-init
                 (stream-map update-on-command commands random-stream)))
  random-stream)

(define request (list->stream '(1 1 1 1 1 0 1 1 1 1 1))) ; 1 for generate, 0 for reset
(display-stream (random-generate request) 10)

;; ========================================================================== 
;; EXERCISE 3.82
;; ========================================================================== 
;; Old version
;; Monte Carlo
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; Integration
(define (estimate-integral trials)
  ;; (random) in racket return inexact number between 0 and 1
  ;; so I code this
  (define (random-in-range) (* 2 (random)))
  (define (integral)
    (let ((x (random-in-range))
          (y (random-in-range)))
      (<= (sqrt (+ (square (- x 1)) (square (- y 1)))) 1)))
  (monte-carlo trials integral))

;; Estimate pi
(* 4 (estimate-integral 1000000.0)) ; 3.14

;; New version
;; NOTICE: THIS VERSION IS SPECIFIED FOR CALCULATE PI, IT'S NOT GENERAL INTEGRAL
;; Monte Carlo
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;; Integration
;; If you want to make generalized version, edit this stream
;; here I generate random number between 0 and 2
(define random-in-range-stream
  (stream-map (lambda (x) (* 2 (random) x)) ones))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define integral-stream
  (map-successive-pairs
    (lambda (x y) (<= (sqrt (+ (square (- x 1)) (square (- y 1)))) 1))
    random-in-range-stream))

(define estimate-integral
  (scale-stream
    (monte-carlo integral-stream 0 0)
    4.0))

;; Estimate pi
(stream-ref estimate-integral 1000) ; 3.14?


