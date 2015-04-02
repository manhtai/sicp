;; EXERCISE 2.1
;; better make-rat

(define (make-rat a b)
  (cond ((>= (* a b) 0) (cons (abs a) (abs b)))
        (else (cons (- (abs a)) (abs b)))))


(define one-minus-half (make-rat 1 -2))
(cdr one-minus-half)
(car one-minus-half)


;; EXERCISE 2.2
;; Segment in a plane
(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (let ((start-point (start-segment segment))
        (end-point (end-segment segment)))
    (make point (average (x-point start-point))
                (average (y-point end-point)))))

(define (len-segment segment)
  (let ((start-point (start-segment segment))
        (end-point (end-segment segment)))
    (sqrt (+ (square (- (x-point start-point) (x-point end-point)))
             (square (- (y-point start-point) (y-point end-point)))))))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; EXERCISE 2.3
;; Rectangle in a plane
(define (make-rec hor-seg ver-seg)
  (cons hor-seg ver-seg))

(define (get-hor rec)
  (car rec))

(define (get-ver rec)
  (cdr rec))

(define (get-per rec)
  (let ((hor (get-hor rec))
        (ver (get-ver rec)))
    (* 2 (+ (len-segment hor) (len-segment ver)))))

(define (get-are rec)
  (let ((hor (get-hor rec))
        (ver (get-ver rec)))
    (* (len-segment hor) (len-segment ver))))

;; Test that!

(define point-a (make-point 0 3))
(define point-b (make-point 0 0))
(define point-c (make-point 4 0))
(define segment-ab (make-segment point-a point-b))
(define segment-bc (make-segment point-b point-c))
(define my-rec (make-rec segment-ab segment-bc))

(get-per my-rec) ; 14
(get-are my-rec) ; 12

;; Another implementation of rectangle
(define (make-rec point-a point-b point-c)
  (let ((segment-ab (make-segment point-a point-b))
        (segment-bc (make-segment point-b point-c)))
    (cons segment-ab segment-bc)))

(define point-a (make-point 0 3))
(define point-b (make-point 0 0))
(define point-c (make-point 4 0))
(define my-rec (make-rec point-a point-b point-c))

(get-per my-rec) ; 14
(get-are my-rec) ; 12

;; EXERCISE 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;; (car (cons x y))
;; ((cons x y) (lambda (p q) p))
;; ((lambda (m) (m x y) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x
;; It works!

(define (cdr z)
  (z (lambda (p q) q))) ; It must work, too !


;; EXERCISE 2.5
;; Wishful thinking: I wish I have a procedure to get power out
;; of the product 2^a.b^3

(define (make-pair a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (get-a pair-ab)
  (get-power pair-ab 2))

(define (get-b pair-ab)
  (get-power pair-ab 3))

;; Now we have to really write it :(
(define (get-power product base)
  (define (divide product pow)
      (if (= (modulo product base) 0)
          (divide (/ product base) (+ pow 1))
          pow))
  (divide product 0))

(define my-pair (make-pair 4 5))
(get-power my-pair 2) ; 4

(get-a my-pair) ; 4
(get-b my-pair) ; 5


;; EXERCISE 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; We try add one to zero:
(add-1 zero)
((lambda (f) (lambda (x) (f ((n f) x)))) zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f x)))

;; We got one!
(define one (lambda (f) (lambda (x) (f x))))

;; Add one to one
(add-1 one)
((lambda (f) (lambda (x) (f ((n f) x)))) one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

;; We got two!
(define two (lambda (f) (lambda (x) (f (f x)))))

;; We can guess three must be:
(lambda (f) (lambda (x) (f (f (f x)))))

;; We define plus such that
;; (plus one two) --> three
;; We see: 
;; (one f) = (lambda (x) (f x))
;; hence ((one f) x) = (f x)
;; similar ((two f) x) = (f (f x))
;; we want (f (f (f x))), this is equivalent to (f ((two f) x)) which has
;; the form (f x), which is equivalent to ((one f) x), so we have
;; ((one f) ((two f) x))
;; Eureka!

(define (plus a b)
  (lambda (a b) (lambda (f) (lambda (x) ((a f) ((b f) x))))))

;; How about multiplication?
;; (one f) = (lambda (x) (f x))
;; (two f) = (lambda (x) (f (f x)))
;; (multi one two) = two, this means we have an interaction between `one` and
;; `two` but nothing change for `two`, what could that be?
;; We have (one f) = (lambda (x) (f x)) so the numbef of `f` is the same after
;; apply 'one' to 'f'. This means that we must apply `one` to (two f) to get
;; the same proceduce take f as an argument.

(define (multi a b)
  (lambda (a b) (lambda (f) (lambda (x) ((a (b f)) x)) f)))






