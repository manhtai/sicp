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
;; f(n) = 2^2^2... (n exponential)

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

;; EXERCISE 1.17

;; EXERCISE 1.18

;; EXERCISE 1.19

;; EXERCISE 1.20

;; EXERCISE 1.21

;; EXERCISE 1.22

;; EXERCISE 1.23

;; EXERCISE 1.24

;; EXERCISE 1.25

;; EXERCISE 1.26

;; EXERCISE 1.27

;; EXERCISE 1.28





































