#lang r5rs

(#%require (only racket/base
                 current-inexact-milliseconds
                 error
                 flush-output
                 make-parameter
                 random
                 void?)
           (rename racket/base racket:module-begin #%module-begin)
           (only (planet williams/science:4:=8/science) random-integer) 
           ;; Import random-integer for generating number larger than 4294967087
           ;; but it require exact integer, so we don't use (random 1e12), 
           ;; we use (random 1000000000000)
           (all-except (planet soegaard/sicp:2:=1/sicp) cons-stream))
           ;; Import sicp picture language for draw picture in racket

;;; Arithmetic

(define true #t)

(define false #f)

(define nil '())

(define (identity x) x)

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (average a b) (/ (+ a b) 2))

(define (cube x) (* x x x))

(define (square x) (* x x))

(define (expmod b exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod b (/ exp 2) m))
          m))
        (else
         (remainder
          (* b (expmod b (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= 0 times) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (fast-prime? n 100))

(define pi/4 (atan 1 1))
(define pi (* 4 pi/4))

;;; Utilities

(define (runtime)
  (inexact->exact (truncate (* 1000 (current-inexact-milliseconds)))))

(define (compose f g)
  (define (f*g x)
    (f (g x)))
  f*g)

(define (repeated f n)
  (cond ((= n 0) identity)
	((= n 1) f)
	(else (compose f (repeated f (- n 1))))))

;;; Streams

(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))

(define the-empty-stream '())

(define (stream-null? x) (null? x))

;;; Syntax

(define-syntax sicp:error
  (syntax-rules ()
    ((_ REASON ARG ...) (error REASON ARG ...))))

(define-syntax sicp-syntax-error
  (syntax-rules ()
    ((_) #f)))

(define-syntax check-expect
  (syntax-rules ()
    ((_ VAL EXPECT)
     (%check-expect-internal 'check-expect
                             equal?
                             (quote VAL)
                             VAL
                             EXPECT))))

(define-syntax check-expect-approx
  (syntax-rules ()
    ((_ VAL EXPECT)
     (%check-expect-internal 'check-expect-approx
                             %approx-equal?
                             (quote VAL)
                             VAL
                             EXPECT))))

(define (%check-expect-internal name check-proc val-syntax val expected)
  (display name)
  (display ": ")
  (write val-syntax)
  (display " \u21D2 ")
  (let ((v VAL))
    (display val)
    (newline)
    (if (check-proc val expected)
        (values)
        (error name
               "Test failed: expected ~S"
               expected))))

(define (%approx-equal? a b)
  (< (abs (- a b)) 1/10000))


(#%provide
 (for-syntax syntax-rules ...)
;; (all-from-except r5rs #%module-begin)
 (rename racket:module-begin #%module-begin)
 (all-from (planet williams/science:4:=8/science))
 (all-from (planet soegaard/sicp:2:=1/sicp))
 (rename sicp:error  error)
 (rename random-integer random)
 check-expect
 check-expect-approx
 false
 true
 identity
 inc
 dec
 nil
 runtime
 square
 cube
 prime?
 pi
 average
 compose
 repeated
 stream-null?
 the-empty-stream
 cons-stream)
