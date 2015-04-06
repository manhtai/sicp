#lang r5rs

(#%require (only racket/base
                 current-inexact-milliseconds
                 error
                 flush-output
                 make-parameter
                 void?)
           (only (planet williams/science:4:=8/science) random-integer) 
           ;; Import random-integer for generating number larger than 4294967087
           ;; but it require exact integer, so we don't use (random 1e12), 
           ;; we use (random 1000000000000)
           (all-except (planet soegaard/sicp:2:=1/sicp) cons-stream))
           ;; Import sicp picture language for draw picture in racket

;;; @some frequent procedures

(define-syntax sicp:error
  (syntax-rules ()
    ((_ REASON ARG ...) (error REASON ARG ...))))

(define true #t)

(define false #f)

(define nil '())

(define (identity x) x)

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (square x) (* x x))

(define (runtime)
  (inexact->exact (truncate (* 1000 (current-inexact-milliseconds)))))

;;; @section Streams

(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))

(define the-empty-stream '())

(define (stream-null? x) (null? x))

;;; @section Syntax

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
 (all-from-except r5rs)
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
 stream-null?
 the-empty-stream
 cons-stream)

