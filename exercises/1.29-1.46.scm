;; EXERCISE 1.29

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;; Simpson's Rule

(define (simpson f a b n)
  (define (add-h x) (+ x (* 2 (/ (- b a) n))))
  (* (/ (/ (- b a) n)
        3)
     (+ (- 0 (f a) (f b))
        (* (sum f (+ a (/ (- b a) n)) add-h b) 4)
        (* (sum f a add-h b) 2))))

(simpson cube 0 1 100) ; 1/4 or so, I don't know :D
(simpson cube 0 1 1000) ; 1/4

;; EXERCISE 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; EXERCISE 1.31
;; a) iterative product

(define (product term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (define (add-one x) (+ x 1))
  (define (identity x) x)
  (product identity 1 add-one n))

(factorial 5) ; 120

(define (prod-pi n)
  (define (add-2 x) (+ x 2))
  (define (frac+1 a) (/ a (+ a 1)))
  (define (frac-1 a) (/ a (- a 1)))
  (* 4.0
     (product frac+1 2 add-2 n)
     (product frac-1 4 add-2 n)))

(prod-pi 1000)

;; b) recursive product

(define (product term a next b)
  (if (< b a)
      1
      (* (term a)
         (product term (next a) next b))))

(factorial 5) ; The same!

;; EXERCISE 1.32
;; More general than product and sum!

(define (accumulate combiner null-value term a next b)
  (if (< b a)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(factorial 5) ; 120?

;; Iterative version

(define (accumulate combiner null-value term a next b)
  (define (acc-iter a result)
    (if (< b a)
        result
        (acc-iter (next a) (combiner (term a) result))))
  (acc-iter a null-value))

(factorial 5) ; 






