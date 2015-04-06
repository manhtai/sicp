#lang r5rs

(#%require (only racket/base
                 current-inexact-milliseconds
                 current-print
                 error
                 flush-output
                 make-parameter
                 random
                 void?)
           (rename racket/base racket:module-begin #%module-begin)
           (only (planet williams/science:4:=8/science) random-integer) 
           ;; Import random-integer for generating number larger than 4294967087
           ;; but it require exact integer, so we don't use (random 1e12), 
           ;; we use (randomm 1000000000000)
           (all-except (planet soegaard/sicp:2:=1/sicp) cons-stream)
           ;; Import sicp picture language for draw picture in racket
           )

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

;; {{{
;; This random can only generate number smaller than 4294967087
;; (define (sicp:random x)
;;   (if (zero? x)
;;       (error 'random
;;              "You called \"(random 0)\".  If you're doing SICP section 1.2.6, don't use 1 for the first argument of \"fast-prime?\".")
;;       (random x)))
;; }}}

;; {{{
;; This causes awful list presentation so I commented it.
;; ;;; @section Streams
;; 
;; (define-syntax cons-stream
;;   (syntax-rules ()
;;     ((_ A B) (cons A (delay B)))))
;; 
;; (define the-empty-stream '())
;; 
;; (define (stream-null? x) (null? x))
;; }}}

;;; {{{
;;; @section

(define-syntax sicp-syntax-error
  (syntax-rules ()
    ((_) #f)))

;; Note: This only works with top-level "define" in PLT.
;;
;; (define-syntax sicp-define
;;   (syntax-rules ()
;;     ((_ A B0 B1 ...)
;;      (sicp-define:1 (define A B0 B1 ...) A))))
;;
;; (define-syntax sicp-define:1
;;   (syntax-rules ()
;;     ((_ DEF (X0 X1 ...))
;;      (sicp-define:1 DEF  X0))
;;     ((_ DEF ())
;;      (%sicp-syntax-error "Invalid define form"))
;;     ((_ DEF X0)
;;      (begin DEF (quote X0)))))

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

;;; }}}

;;; {{{
;;; @section Print Handler

;; (define %current-print-handler-ate-first-void? (make-parameter #f))
;; 
;; (define (%make-print-handler)
;;   (let ((skip-void? #t))
;;     (lambda (val)
;;       (let ((sv? skip-void?))
;;         (set! skip-void? #f)
;;         (or (and sv? (void? val))
;;             (let ((out (current-output-port)))
;;               (display "[PRINT] " out) ; DEBUG
;;               (write val out)
;;               (newline out)
;;               (flush-output out)))))))
;; 
;; (current-print (%make-print-handler))

;;; }}}

(#%provide
 (for-syntax syntax-rules ...)
 (all-from-except r5rs #%module-begin)
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
;; stream-null?
;; the-empty-stream
;; cons-stream
)
