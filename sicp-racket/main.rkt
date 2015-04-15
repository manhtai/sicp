#lang racket

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
           racket/mpair
           ;; mpair for set-mcar!, set-mcdr!
           graphics/graphics)
           ;; Import graphics for display picture in canvas

;;; ARITHMETIC

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

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))


;;; UTILITIES

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

;;; STREAMS

(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))

(define the-empty-stream '())

(define (stream-null? x) (null? x))

;;; Syntax checking, I don't know what it is yet

;; (define-syntax sicp:error
;;   (syntax-rules ()
;;     ((_ REASON ARG ...) (error REASON ARG ...))))
;; 
;; (define-syntax sicp-syntax-error
;;   (syntax-rules ()
;;     ((_) #f)))
;; 
;; (define-syntax check-expect
;;   (syntax-rules ()
;;     ((_ VAL EXPECT)
;;      (%check-expect-internal 'check-expect
;;                              equal?
;;                              (quote VAL)
;;                              VAL
;;                              EXPECT))))
;; 
;; (define-syntax check-expect-approx
;;   (syntax-rules ()
;;     ((_ VAL EXPECT)
;;      (%check-expect-internal 'check-expect-approx
;;                              %approx-equal?
;;                              (quote VAL)
;;                              VAL
;;                              EXPECT))))
;; 
;; (define (%check-expect-internal name check-proc val-syntax val expected)
;;   (display name)
;;   (display ": ")
;;   (write val-syntax)
;;   (display " \u21D2 ")
;;   (let ((v VAL))
;;     (display val)
;;     (newline)
;;     (if (check-proc val expected)
;;         (values)
;;         (error name
;;                "Test failed: expected ~S"
;;                expected))))

(define (%approx-equal? a b)
  (< (abs (- a b)) 1/10000))

;;; CANVAS FOR DIPLAYING PICTURE FROM RACKET REPL

;;; Primitives

;; vect
(define (make-vect x y) (cons x y))
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect vec1 vec2)
  (make-vect (+ (xcor-vect vec1) (xcor-vect vec2)) (+ (ycor-vect vec1) (ycor-vect vec2))))

(define (sub-vect vec1 vec2)
  (make-vect (- (xcor-vect vec1) (xcor-vect vec2)) (- (ycor-vect vec1) (ycor-vect vec2))))

(define (scale-vect s vec)
  (make-vect (* s (xcor-vect vec)) (* s (ycor-vect vec))))

;; segment
(define (make-segment start-vec end-vec) (list start-vec end-vec))
(define start-segment car)
(define end-segment cadr)

;; frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; painter
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
      (line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

;;; Canvas

;; set size for canvas
(define canvas-margin 4)
(define canvas-width  512)
(define canvas-height 512)

;; view point
(define vp nil) 

;; open canvas to display picture
(define open-canvas 
  (lambda ()
    (if (null? vp)
      (begin
        (open-graphics)
        (set! vp (open-viewport "A Picture Language"
                                (+ canvas-width  (* canvas-margin 2))
                                (+ canvas-height (* canvas-margin 2)))))
      nil)))

;; close canvas
(define close-canvas  
  (lambda ()
    (if (null? vp)
        nil
        (begin
          (close-viewport vp)
          (close-graphics)
          (set! vp nil)))))

;; clear canvas for another picture
(define clear-canvas   
  (lambda ()
    (if (null? vp)
        nil
        ((clear-viewport vp)))))

;; draw line on canvas
(define (line start-vec end-vec)
  (define (vect->posn vec)
    (make-posn (xcor-vect vec) (ycor-vect vec)))
  ((draw-line vp) (vect->posn start-vec)
                  (vect->posn end-vec)))

;; draw painter on canvas
(define (draw painter)
  (let ((f (make-frame
            (make-vect canvas-margin
                       (+ canvas-margin canvas-height))
            (make-vect canvas-width 0)
            (make-vect 0 (* -1 canvas-height)))))
    (painter f)))

;;; Geogre
(define geogre
   (list (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.4))
         (make-segment (make-vect 0.4 0.4) (make-vect 0.3 0.5))
         (make-segment (make-vect 0.3 0.5) (make-vect 0.1 0.3))
         (make-segment (make-vect 0.1 0.3) (make-vect 0.0 0.6))
         (make-segment (make-vect 0.0 0.8) (make-vect 0.1 0.5))
         (make-segment (make-vect 0.1 0.5) (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
         (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
         (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1.0))
         (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.8))
         (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
         (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.6))
         (make-segment (make-vect 0.8 0.6) (make-vect 1.0 0.4))
         (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.4))
         (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.0))
         (make-segment (make-vect 0.7 0.0) (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0.0))))

(define wave
  (segments->painter geogre))

;;; DISPATCH FOR DATA-DIRECTED PROGRAMMING

;; Some primitives
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; for method dispatch table
(define DISPATCH-TABLE (make-hash))

(define (put op type proc)
  (hash-set! DISPATCH-TABLE (cons op type) proc))

(define (get op type)
  (hash-ref DISPATCH-TABLE (cons op type) #f))

(define (dispatch-clear!)
  (hash-clear! DISPATCH-TABLE))

;; for type-tags
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;; the dispatch methods
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(#%provide
;; (for-syntax syntax-rules ...)
 (rename racket:module-begin #%module-begin)
 (all-from (planet williams/science:4:=8/science))
 (all-from graphics/graphics)
 (all-from racket/mpair)
;; (rename sicp:error  error)
;; check-expect
;; check-expect-approx
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
 open-canvas
 clear-canvas
 close-canvas
 draw
 line
 wave
 geogre
 stream-null?
 the-empty-stream
 cons-stream
 get
 put
 attach-tag
 type-tag
 contents
 variable?
 same-variable?
 apply-generic)

