;; EXERCISE 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; Recusive procedure, iterative process

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
;; Recusive procedure, recusive process

;; EXERCISE 1.10
;; Ackermann’s function
;; This function has one of many applications that is used in Union Find with 
;; rank and path compression. It's inverse function is called alpha.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10)
;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 ..9.. (A 1 1))
;; (A 0 ..8.. (A 0 2))
;; (A 0 ..7.. (A 0 4))
;; 2^10

(A 2 4)
;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 0 (A 1 3))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 2)))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 2)))))
;; (A 1 (A 0 (A 0 (A 0 4))))
;; (A 1 (A 0 (A 0 8)))
;; (A 1 (A 0 16))
;; (A 1 32)
;; (A 0 (A 1 31))
;; ...
;; 2^16

(A 3 3)
;; 2^16

(define (f n) (A 0 n))
;; (A 0 n) = 2n

(define (g n) (A 1 n))
;; (A 1 n) = (A 0 (A 1 n-1))
;; f(n) = 2*f(n-1)
;; f(n) = 2^n

(define (h n) (A 2 n)) 
;; (A 1 (A 2 n-1))
;; 2^f(n-1) = f(n)
;; f(n) = 2^2^2... (2 power iteration)

(define (k n) (* 5 n n))
;; 5n^2

;; EXERCISE 1.11

;; Recursive version
(define (f-re n)
  (if (< n 3)
      n
      (+ (f-re (- n 1))
         (* 2 (f-re (- n 2)))
         (* 3 (f-re (- n 3))))))

;; Iterative version
(define (f-it n)
  (define (f-it-iter n1 n2 n3 count)
    (if (= count 0)
        n1
        (f-it-iter (+ n1
                      (* 2 n2)
                      (* 3 n3)) 
                   n1 
                   n2
                   (- count 1))))
  (if (< n 3)
      n
      (f-it-iter 2 1 0 (- n 2))))

(f-re 10) ; 1892
(f-it 10) ; The same!

;; EXERCISE 1.12:
;; Pascal's triangle

(define (pascal row col)
  (cond ((and (> row 0) (= col 0)) 0)
        ((and (= row 0) (= col 0)) 1)
        ((> col row) 0)
        (else (+ (pascal (dec row) col)
                 (pascal (dec row) (dec col))))))
(pascal 1 1) ; 1
(pascal 5 3) ; 6

;; EXERCISE 1.13
;; You actually can calculate exactly Fib(n) value using Linear Algebra
;; Long live Linear Algebra! Hehe.

;; EXERCISE 1.14
;; I'm lazy :(

;; EXERCISE 1.15
;; a) Round up of log base 3 of (12.15/0.1)
;; = 5 times

;; b) 
;; space: O(1), this is a iterative process
;; step: O(log(a)), as we calculate above


;; EXERCISE 1.16
;; Logarithmic recursive process
(define (fast-expt b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(fast-expt 2 5)

;; Now convert to iterative process
(define (iter-expt b n)
  (define (iter-iter b a n)
    (if (= n 0)
        a
        (if (even? n) 
            (iter-iter 
              (square b) 
              (* a (square b))
              (- (/ n 2) 1))
            (iter-iter
              b
              (* a b)
              (- n 1)))))
  (define (square x) (* x x))
  (iter-iter b 1 n))

(iter-expt 2 5) ; 32

;; EXERCISE 1.17
;; O(n)
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
;; O(log n)
(define (fast-prod a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (if (= a 1)
      b
      (if (even? a)
          (fast-prod (halve a) (double b))
          (+ (fast-prod (- a 1) b) b))))

(fast-prod 5 2) ; 10

;; EXERCISE 1.18
;; Convert procedure in exercise 1.17 to iterative process
(define (iter-prod a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (prod-iter a b c)
    (if (= a 0)
        c
        (if (even? a)
            (prod-iter (halve a) (double b) c)
            (prod-iter (- a 1) b (+ c b)))))
  (prod-iter a b 0))

(iter-prod 5 2) ; The same!

;; EXERCISE 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* p q 2) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 9) ; 34, indeed!


;; EXERCISE 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Normal order evaluation
;; (gcd 206 40) 
;; (gcd 40 (remainder 206 40))
;; (gcd (remainder 206 40) (remainder (40 (remainder 206 40))))
;; (gcd 6 (remainder (40 (remainder 206 40)))) => +1
;; (gcd (remainder (40 (remainder 206 40))) (remainder ((remainder (40 (remainder 206 40))) 6)))
;; (gcd 4 (remainder ((remainder (40 (remainder 206 40))) 6))) => +2
;; (gcd (remainder ((remainder (40 (remainder 206 40))) 6)) (remainder (remainder ((remainder (40 (remainder 206 40))) 6) 4)))
;; (gcd 2 (remainder (remainder ((remainder (40 (remainder 206 40))) 6) 4)) => +3
;; (gcd 2 (...)) => +4
;; (gcd 2 0) => +5, I guess :D 
;; 2
;; => Evaluated ~15 remainder, I'm not very sure, but I'm very sure it is 
;; more than application order evaluation

;; Application order evaluation
;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd 40 6)
;; (gcd 6 (remainder 40 6))
;; (gcd 6 4)
;; (gcd 4 (remainder 6 4))
;; (gcd 4 2)
;; (gcd 2 (remainder 4 2))
;; (gcd 2 0)
;; 2
;; => Evaluated 4 remainder

;; EXERCISE 1.21

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(smallest-divisor 199) ; 199
(smallest-divisor 1999) ; 1999
(smallest-divisor 19999); 7

;; EXERCISE 1.22
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n  elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n count)
  (define (search x count)
    (if (> count 0)
        (if (timed-prime-test x)
            (search (+ x 2) (- count 1))
            (search (+ x 2) count))))
  (if (even? n)
      (search (+ n 1) count)
      (search n count)))

(define (print-time start stop)
  (cond ((< start stop)
         (newline)
         (display "Prime larger than ")
         (display start)
         (search-for-primes start 3)
         (print-time (* 10 start) stop)
         (newline))))

(print-time 1e12 1e15)

;; Prime larger than 1000000000000.0
;; 1000000000039.0 *** 190413
;; 1000000000061.0 *** 176692
;; 1000000000063.0 *** 180950

;; Prime larger than 10000000000000.0
;; 10000000000037.0 *** 571824
;; 10000000000051.0 *** 574216
;; 10000000000099.0 *** 574003

;; Prime larger than 1e+14
;; 100000000000031.0 *** 1838566
;; 100000000000067.0 *** 1831935
;; 100000000000097.0 *** 1857066

;; sqrt(10) ~ 3.1
;; You can see it.

;; EXERCISE 1.23
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next test)
    (if (= test 2)
        3
        (+ test 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n  elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n count)
  (define (search x count)
    (if (> count 0)
        (if (timed-prime-test x)
            (search (+ x 2) (- count 1))
            (search (+ x 2) count))))
  (if (even? n)
      (search (+ n 1) count)
      (search n count)))

(define (print-time start stop)
  (cond ((< start stop)
         (newline)
         (display "Prime larger than ")
         (display start)
         (search-for-primes start 3)
         (print-time (* 10 start) stop)
         (newline))))

(print-time 1e12 1e15)

;; Prime larger than 1000000000000.0
;; 1000000000039.0 *** 102073
;; 1000000000061.0 *** 88808
;; 1000000000063.0 *** 88729
;; Prime larger than 10000000000000.0
;; 10000000000037.0 *** 283789
;; 10000000000051.0 *** 285513
;; 10000000000099.0 *** 284948
;; Prime larger than 1e+14
;; 100000000000031.0 *** 911743
;; 100000000000067.0 *** 913707
;; 100000000000097.0 *** 917835


;; EXERCISE 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(require (planet williams/science/random-source)) 
;; for using random-integer, support generate random
;; number larger than 4294967087

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 100)) ; Is 100 times enough?

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n  elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n count)
  (define (search x count)
    (if (> count 0)
        (if (timed-prime-test x)
            (search (+ x 2) (- count 1))
            (search (+ x 2) count))))
  (if (even? n)
      (search (+ n 1) count)
      (search n count)))

(define (print-time start stop)
  (cond ((< start stop)
         (newline)
         (display "Prime larger than ")
         (display start)
         (search-for-primes start 3)
         (print-time (* 10 start) stop)
         (newline))))

(print-time 1000000000000 1000000000000000) ; random-integer require exact integer

;; Prime larger than 1000000000000
;; 1000000000039 *** 4858
;; 1000000000061 *** 4666
;; 1000000000063 *** 4868
;; Prime larger than 10000000000000
;; 10000000000037 *** 6270
;; 10000000000051 *** 4069
;; 10000000000099 *** 4256
;; Prime larger than 100000000000000
;; 100000000000031 *** 4765
;; 100000000000067 *** 4476
;; 100000000000097 *** 4217

;; EXERCISE 1.25
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; The goal of reduction to calculate the expmod in the first procedure
;; is to deal with much smaller number, take their modulo and multiply them
;; with each others.

;; This new expmod just use reduction for calculate exponential, not for
;; calculate modulo itself, hence, it's too expensive!


;; EXERCISE 1.26
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; the (expmode base (/ exp 2) m) is recursive process, if Louis use multiplication
;; the computer must calculate this element twice before multiply two equal things.
;; If he use square procedure, it only must calculate once, and multiply with itself.
;; The size doesn't change. Hence O(log n) and O(n)

;; EXERCISE 1.27
;; Carmichael numbers
(define (expmod base exp m)
  (define (square x) (* x x))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (foo-test n)
  (define (try-it a)
    (if (= a 0)
        #t
        (if (= (expmod a n n) a)
            (try-it (- a 1))
            #f)))
  (try-it (- n 1)))

(foo-test 561) ; #t
(foo-test 1105) ; #t
(foo-test 1729) ; #t
(foo-test 2465) ; #t
(foo-test 2821) ; #t
(foo-test 6601) ; #t

;; EXERCISE 1.28
;; Miller-Rabin test

(define (expmod base exp m)
  (define (square x) (* x x))
  (define (signal x)
    (if (and (not (or (= x 1) (= x (- m 1))))
             (= 1 (remainder (square x) m)))
        0
        x))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (signal (expmod base (/ exp 2) m)))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n (if (even? n)
                     (/ n 2)
                     (/ (- n 1) 2))))


(prime? 561) ; #f
(prime? 1105) ; #f
(prime? 1729) ; #f
(prime? 2465) ; #f
(prime? 2821) ; #f
(prime? 6601) ; #f






