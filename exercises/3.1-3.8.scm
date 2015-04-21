;; EXERCISE 3.1
(define (make-accumulator init)
  (lambda (x)
    (begin (set! init (+ init x)) init)))

(define A (make-accumulator 5))
(A 10) ; 15
(A 10) ; 25


;; EXERCISE 3.2
(define (make-monitored f)
  (let ((x 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) x)
            ((eq? m 'reset-count) (set! x 0))
            ((number? m) (begin (set! x (inc x)) (f m)))))
    dispatch))

(define s (make-monitored sqrt))
(s 100) ; 10
(s 'how-many-calls?) ; 1
(s 100) ; 10
(s 'how-many-calls?) ; 2
(s 'reset-count)
(s 'how-many-calls?) ; 0


;; EXERCISE 3.3
(define (make-account balance pass)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p pass)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (x) (display "Invalid password"))))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ; 60
((acc 'some-other-password 'deposit) 50) ; "Invalid password"
((acc 'secret-password 'withdraw) 40) ; 20

;; EXERCISE 3.4
(define (make-account balance pass)
  (let ((count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops x)
      (display "The cops are notified!"))
    (define (invalid x)
      (display "Invalid password"))
    (define (dispatch p m)
        (if (eq? p pass)
            (begin (set! count 0) 
                   (cond ((eq? m 'withdraw) withdraw)
                         ((eq? m 'deposit) deposit)
                         (else (error "Unknown request -- MAKE-ACCOUNT"
                                       m))))
            (begin (set! count (inc count))
                   (if (< count 7)
                       invalid
                       call-the-cops))))
    dispatch)) ;; The dispatch must be encapsulated in (let ((count 0)) ... )

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ; 60
((acc 'some-other-password 'deposit) 50) ; "Invalid password"
((acc 'some-other-password 'deposit) 50) ; "Invalid password"
((acc 'some-other-password 'deposit) 50) ; "Invalid password"
((acc 'some-other-password 'deposit) 50) ; "Invalid password"
((acc 'some-other-password 'deposit) 50) ; "Invalid password"
((acc 'some-other-password 'deposit) 50) ; "Invalid password"
((acc 'some-other-password 'deposit) 50) ; "Invalid password"
((acc 'some-other-password 'deposit) 50) ; "The cops are notified!"
((acc 'secret-password 'withdraw) 40) ; 20
((acc 'some-other-password 'deposit) 50) ; "Invalid password"


;; EXERCISE 3.5
;; Monte Carlo integration
;; NOTICE: THIS VERSION IS SPECIFIED FOR CALCULATE PI, IT'S NOT GENERAL INTEGRAL
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
(* 4 (estimate-integral 1000000.0)) ; 3.14?



;; EXERCISE 3.6
;; support rand-update
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

;; rand generator
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
;; 'dispatch' and 'rand' is 2 diffirent things: we we call 'rand' it will
;; invoke 'dispatch' which is modify x inside 'let' scope
;; MORE in section 3.2
              

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 7)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

;; EXERCISE 3.7
;; make-account
(define (make-account balance pass)
  (let ((pass2 null))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (add-pass p)
    (set! pass2 p))
  (define (dispatch p m)
    (if (and (not (null? p)) (or (eq? p pass) (eq? p pass2)))
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'balance) balance)
              ((eq? m 'add-pass) add-pass)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (x) (display "Invalid password"))))
  dispatch))

; make-joint
(define (make-joint acc1 pass1 pass2)
  (define acc2 acc1)
  ((acc1 pass1 'add-pass) pass2)
  acc2)

;; Test that!
;; Peter's
(define peter-acc (make-account 100 'open-sesame))

;; Paul's is joined with Peter
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

;; Now withdraw and deposit
((peter-acc 'open-sesame 'withdraw) 40) ; 60
((paul-acc 'rosebud 'withdraw) 40) ; 20
((paul-acc 'rosebud 'deposit) 80) ; 100
((peter-acc 'open-sesame 'withdraw) 40) ; 60

;; EXERCISE 3.8
;; Evaluation order
(define f
  (let ((one 1))
    (lambda (x)
      (set! one (* x one))
      one)))

(+ (f 0) (f 1)) ; 0
;; This f is the same
(define f
  (let ((one 1))
    (lambda (x)
      (set! one (* x one))
      one)))

(+ (f 1) (f 0)) ; 1



