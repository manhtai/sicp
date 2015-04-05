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

;; ===============================================================
;; EXERCISE 2.7
;; Alyssa's work
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; My work
(define (make-interval a b) (cons a b))
(define (upper-bound interval)
  (max (car interval) (cdr interval)))
(define (lower-bound interval)
  (min (car interval) (cdr interval)))

;; EXERCISE 2.8
(define (sub-interval x y)
  (add-interval
    x
    (make-interval (- (upper-bound y))
                   (- (lower-bound y)))))

;; EXERCISE 2.9
;; a_int = a_low + a_up
;; a_wid = (a_up - a_low) / 2
;; b_int = b_low + b_up
;; b_wid = (b_up - b_low) / 2
;; We have
;; a+b_int = (a_low + b_low) + (a_up + b_up)
;; a+b_wid = (a_up + b_up - a_low - b_low) / 2
;; a+b_wid = a_wid + b_wid
;; QED
;; Counter example for multiplication
;; (3, 7) width 2
;; (4, 6) width 1
;; (8, 10) width 1
;; (3, 7) x (4, 6) = (12, 42) width 15
;; (3, 7) x (8, 10) = (24, 70) width 23

;; EXERCISE 2.10
(define (div-interval x y)
  (if (span-zero? y)
      (display "y spans zero")
      (mul-interval
                x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))
(define (span-zero? x)
  (let ((up (upper-bound x))
        (low (lower-bound x)))
    (or (= 0 up)
        (= 0 low)
        (> 0 (* low up)))))

;; EXERCISE 2.11
;; The signs of the enpoints of one interval can be [+,+], [-,+], [-,-]
;; With 2 intervals we have 3x3 = 9 cases and the case with [-,+] and [-,+]
;; is required more than 2 multiplication (4 exactly)
;; We call
;; [+,+] sign 1
;; [-,-] sign -1
;; [-,+] sign 0

(define (get-sign x)
  (let ((lowx (lower-bound x))
        (upx (upper-bound x)))
    (cond ((>= lowx 0) 1)
          ((<= upx 0) -1)
          (else 0))))
;; We call
;; [+,+] [+,+] and [-,-] [-,-]: case 1 "uu-ll"
;; [-,-] [+,+] and [+,+] [-,-]: case 2 "ul-lu"
;; [+,+] [-,+]: case 3 "uu-ul"
;; [-,+] [+,+]: case 4 "uu-lu"
;; [-,-] [-,+]: case 5 "lu-ll"
;; [-,+] [-,-]: case 6 "ll-ul"
;; [-,+] [-,+]: case 7 "xx-xx", this is the case required 4 multiplications
;; Now we got 7 cases instead of 9 cases!

(define (get-case x y)
  (let ((x-sign (get-sign x))
        (y-sign (get-sign y)))
    (cond  ((or (and (= x-sign 1) (= y-sign 1))
                (and (= x-sign -1) (= y-sign -1)))
            1)
           ((or (and (= x-sign -1) (= y-sign 1))
                (and (= x-sign 1) (= y-sign -1)))
            2)
           ((and (= x-sign 1) (= y-sign 0))
            3)
           ((and (= x-sign 0) (= y-sign 1))
            4)
           ((and (= x-sign -1) (= y-sign 0))
            5)
           ((and (= x-sign 0) (= y-sign -1))
            6)
           (else 7))))

;; Phew... New we define multiplication
(define (weird-multi x y)
  (let ((weird-case (get-case x y))
        (ux (upper-bound x))
        (lx (lower-bound x))
        (uy (upper-bound y))
        (ly (lower-bound y)))
    (cond ((= weird-case 1)
           (make-interval (* ux uy) (* lx ly)))
          ((= weird-case 2)
           (make-interval (* ux ly) (* lx uy)))
          ((= weird-case 3)
           (make-interval (* ux uy) (* ux ly)))
          ((= weird-case 4)
           (make-interval (* ux uy) (* lx uy)))
          ((= weird-case 5)
           (make-interval (* lx uy) (* lx ly)))
          ((= weird-case 6)
           (make-interval (* lx ly) (* ux ly)))
          (else (make-interval (min (* ux uy) (* lx ly))
                               (max (* ux ly) (* lx uy)))))))
;; Damn you, Ben...

(define (equal-i? i1 i2)
  (and (= (lower-bound i1) (lower-bound i2))
       (= (upper-bound i1) (upper-bound i2))))


(define p12 (make-interval 1 2))
(define n12 (make-interval -1 -2))
(define m11 (make-interval -1 1))

(equal-i? (mul-interval p12 p12) (weird-multi p12 p12)) ; #t
(equal-i? (mul-interval p12 n12) (weird-multi p12 n12)) ; #t
(equal-i? (mul-interval p12 m11) (weird-multi p12 m11)) ; #t

(equal-i? (mul-interval n12 p12) (weird-multi n12 p12)) ; #t
(equal-i? (mul-interval n12 n12) (weird-multi n12 n12)) ; #t
(equal-i? (mul-interval n12 m11) (weird-multi n12 m11)) ; #t

(equal-i? (mul-interval m11 p12) (weird-multi m11 p12)) ; #t
(equal-i? (mul-interval m11 n12) (weird-multi m11 n12)) ; #t
(equal-i? (mul-interval m11 m11) (weird-multi m11 m11)) ; #t

(span-zero? m11)


;; EXCERCISE 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-interval (- c w) (+ c w))))

(define (percent i)
  (let ((w (/ (+ (lower-bound i) (upper-bound i)) 2)))
    (* (/ w (center i)) 100)))

(define X (make-center-width 10 1))
(define Y (make-center-percent 10 10))
(equal-i? X Y) ; #t


;; EXERCISE 2.13
;; There is a simple formula to approximate percentage tolerance of
;; the product of 2 intervals, by omitting something very small. 
;; Assume we have 2 intervals:
;; - One: center c1, percentage tolerance p1
;; - Two: center c2, percentage tolerance p2
;; We also assume they are all positive, for simplicity's sake. We have:
;; One * Two = Prod with upper-bound c1(1 + p1)c2(1 + p2) and lower-bound
;; c1(1-p1)c2(1-p2). Percentage tolerance of Prod is:
;; (c1(1+p1)c2(1+p2) - c1(1-p1)c2(1-p2)) / (c1(1+p1)c2(1+p2) + c1(1-p1)c2(1-p2))
;; = 2c1c2(p1 + p2) / 2c1c2(1 + p1p2)
;; = (p1 + p2) / (1 + p1p2)
;; This is exact product, if we want approximate we omit p1p2 and get
;; p = p1 + p2

;; EXERCISE 2.14
;; parallel-resistors

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))


(define t1 (make-center-percent 100 0.001))
(define t2 (make-center-percent 200 0.0001))

(define test1 (par1 t1 t2))
(define test2 (par2 t1 t2))

(equal-i? test1 test2) ; Expected: #t but got #f

;; We check
(upper-bound test1)
(upper-bound test2)
;; and
(lower-bound test1)
(lower-bound test2)

;; We see that they are approximate one another so this is
;; rounded problem when excute arithmetic expresstion, especially
;; division, and multiplication, I guess

;; EXERCISE 2.15
;; Yes, Eva is right
;; Because the less we use the approximate variables for calculate, 
;; the more accurate results we got.

;; EXERCISE 2.16
;; You must take a number theory class to get this explained fully!
;; Done for now, so...







