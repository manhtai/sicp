;; ==========================================================================  
;; GENERIC OPERATIONS
;; ==========================================================================  

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make '(rational)
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

;; EXERCISE 2.77
;; Required packages
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Build complex package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ;; Alyssa idea
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

;; Install package to system
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;; Add some new operations
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Add new generic operation
(define (magnitude x) (apply-generic 'magnitude x))

;; Test z
(define z (make-complex-from-real-imag 3 4))

(magnitude z) ; 5

;; z = '(complex rectangular 3 . 4)
;; (magnitude z)
;; (apply-generic 'magnitude z)
;;    type-tags = (type-tag z) = 'complex
;;    proc = (get 'magnitude 'complex) 
;; If we don't put magnitude operation into complex type it will raise an error
;; If we put magnitude into complex type then:
;;    proc = magnitude
;;       (contents z) = (rectangular 3 . 4)
;; We apply magnitude operation to content of z
;; (magnitude (rectangular 3 . 4))
;; (apply-generic 'magnitude (rectangular 3 . 4))
;;    type-tags = 'rectangular
;;    proc = (get 'magnitude 'rectangular) = (sqrt ...)
;; Now the proc is evaluate directly and give us result 5

;; => Two times 'apply-generic' invoked

;; EXERCISE 2.78:
;; New tag system
(define (attach-tag type-tag contents)
  (if (number? contents) 
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (error "Bad tagged datum -- CONTENTS" datum)))

(make-scheme-number 8)

;; EXERCISE 2.79
;; 'equ?' generic predicate
(define (equ? a b) (apply-generic 'equ? a b))

(define (install-equ-package)
  ;; Update for ordinary numbers
  (define ordinary-equ? =)
  ;; Update for rational numbers
  (define numer car)
  (define denom cdr)
  (define (rational-equ? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))
  ;; Update complex package
  (define (complex-equ? a b)
    (and
     (= (real-part a) (real-part b))
     (= (imag-part a) (imag-part b))))
  ;; Put
  (put 'equ? '(scheme-number scheme-number) ordinary-equ?)
  (put 'equ? '(rational rational) rational-equ?)
  (put 'equ? '(complex complex) complex-equ?))

(install-equ-package)

;; Helper
(define (real-part x) (apply-generic 'real-part x))
(define (imag-part x) (apply-generic 'imag-part x))

;; Test
(equ? (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4))
(equ? (make-scheme-number 10) (make-scheme-number 10))
(equ? (make-rational 10 5) (make-rational 2 1))
(equ? (make-rational 10 5) (make-rational 3 1))


;; EXERCISE 2.80
;; '=zero?' generic predicate
(define (=zero? x) (apply-generic '=zero? x))

(define (install-zero-package)
  ;; Update for ordinary numbers
  (put '=zero? '(scheme-number) zero?)
  ;; Update for rational numbers
  (put '=zero? '(rational) (lambda (x) (zero? (car x))))
  ;; Update complex package
  (put '=zero? '(complex) (lambda (x) (= (real-part x) (imag-part x) 0))))

(install-zero-package)

;; Test
(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-scheme-number 0))
(=zero? (make-rational 0 1))
(=zero? (make-rational 1 1))

;; => Think about 2.76:
;; When new operation arrives, with data-directed approach we must write a package to update
;; it to all old correspoding data types
;; We will talk about this more latter ;)

;; ==========================================================================  
;; COERCION
;; ==========================================================================  

;; EXERCISE 2.81

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)
;; a)
;; If we call exp with two complex numbers:
;; (exp (complex a) (complex b))
;; Since there is no 'exp' procedure for 'complex' type in the table, apply-generic
;; will look up in coercion table and find out 'complex->complex' coercion, it will try
;; to apply it, and the expression becomes
;; (exp (complex a) (complex b))
;; which is same as before => This will cause an infinite loop.

;; b)
;; 'apply-generic' works correctly as it does. We don't need to add anything, but if
;; we still want it doesn't try coercion if 2 arguments have the same type, we can do c)

;; c)
;;  
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2) ;; Test for type, if they are same, raise error
                    (error "No method for these types" (list op type-tags)) 
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; EXERCISE 2.82
;; Multiple arguments version
;; Idea: attempt to coerce all the arguments to the type of the first argument,
;; then to the type of the second argument, and so on.
;; I leave it here as a challenge


;; EXERCISE 2.83
;; Define generic raise
(define (raise x) (apply-generic 'raise x))

;; complex
;;   ^
;; real
;;   ^
;; rational
;;   ^
;; integer

;; Make a dispatched package
(define (install-raise-package)
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'raise '(rational) (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'raise '(real) (lambda (x) (make-from-real-imag x 0))))

(install-raise-package)

;; Note: to use this pacakge you need to build integer and real package beside complex package
;; which we've developed

;; EXERCISE 2.84
;; Raise type in a tower
(define (apply-generic op . args)
  ;; raise l to h
  (define (raise-to l h)
    (let ((l-type (type-tag l))
          (h-type (type-tag h)))
      (cond ((equal? l-type h-type) l)
            ((get 'raise (list l-type))
             (raise-to ((get 'raise (list l-type))) (contents l)) h)
            (else #f))))
  ;; from now on it is same as 2.81
  ;; so everything is work consistent with the rest
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2) ;; Test for type, if they are same, raise error
                    (error "No method for these types" (list op type-tags)) 
                    (let ((t1->t2 (raise-to type1 type2))
                          (t2->t1 (raise-to type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; EXERCISE 2.85
;; Define generic drop
(define (drop x) (apply-generic 'drop x))

;; complex
;;   𝈍
;; real
;;   𝈍
;; rational
;;   𝈍
;; integer

;; Make a dispatched package
(define (install-drop-pacakge)
  (put 'drop '(complex) (lambda (x) (make-real (real-part x))))
  (put 'drop '(real) (lambda (x) (make-integer (floor x))))
  (put 'drop '(rational) (lambda (x) (make-integer (floor (/ (numer x) (denom x)))))))

(install-drop-package)

;; Note: to use this pacakge you need to build integer and real package beside complex package
;; which we've developed

;; Now we use in to rewrite apply-generic
(define (apply-generic op . args)
  ;; raise l to h
  (define (raise-to l h)
    (let ((l-type (type-tag l))
          (h-type (type-tag h)))
      (cond ((equal? l-type h-type) l)
            ((get 'raise (list l-type))
             (raise-to ((get 'raise (list l-type))) (contents l)) h)
            (else #f))))
  ;; drop h to l
  (define (drop-to h l)
    (let ((l-type (type-tag l))
          (h-type (type-tag h)))
      (cond ((equal? l-type h-type) l)
            ((get 'drop (list h-type))
             (drop-to ((get 'drop (list h-type))) (contents h)) l)
            (else #f))))
  ;; dropable?
  (define (dropable? h l)
    (equ? h (raise-to (drop-to h l) h)))
  ;; from now on it is same as 2.81
  ;; so everything is work consistent with the rest
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2) ;; Test for type, if they are same, raise error
                    (error "No method for these types" (list op type-tags)) 
                    (let ((t1->t2? (dropable? type1 type2))
                          (t2->t1? (dropable? type2 type1))
                          (t1->t2 (raise-to type1 type2))
                          (t2->t1 (raise-to type2 type1)))
                      (cond (t1->t2?
                             (apply-generic op ((drop-to type1 type2) a1) a2))
                            (t2->t1?
                             (apply-generic op a1 ((drop-to type2 type1) a2)))
                            (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; EXERCISE 2.86
;; To accomodate the changes:
;; - We need to change +, -, *, / to add, sub, mul, div generic operations
;; - We need to implement square, square-root, sine, cosine, arctan generic operations
;; And apply that to 3 packages: rectangular, polar, complex 

(define (cosine x) (apply-generic 'cosine x))
(define (sine x) (apply-generic 'sine x))
(define (arctan x) (apply-generic 'arctan x))
(define (square-root x) (apply-generic 'square-root x))
(define (square x) (apply-generic 'square x))


;; I leaves implementation details to Geogre



;; ==========================================================================  
;; SYMBOLIC ALGEBRA
;; ==========================================================================  

;; EXERCISE 2.87
(define (install-poly-zero-package)
  ;; Update for polynomials numbers
  (put '=zero? '(polynomials) (lambda (x) (zero? (car x)))))

(install-zero-package)

;; EXERCISE 2.88


;; EXERCISE 2.89


;; EXERCISE 2.90


;; EXERCISE 2.91

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     ??FILL-THIS-IN?? ;compute rest of result recursively
                     ))
                ??FILL-THIS-IN?? ;form complete result
                ))))))


;; EXERCISE 2.93
;: (define p1 (make-polynomial 'x '((2 1)(0 1))))
;: (define p2 (make-polynomial 'x '((3 1)(0 1))))
;: (define rf (make-rational p2 p1))

;; Rational functions

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))


;; EXERCISE 2.94
;: (define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
;: (define p2 (make-polynomial 'x '((3 1) (1 -1))))
;: (greatest-common-divisor p1 p2)


;; EXERCISE 2.97

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

;: (define p1 (make-polynomial 'x '((1 1)(0 1))))
;: (define p2 (make-polynomial 'x '((3 1)(0 -1))))
;: (define p3 (make-polynomial 'x '((1 1))))
;: (define p4 (make-polynomial 'x '((2 1)(0 -1))))

;: (define rf1 (make-rational p1 p2))
;: (define rf2 (make-rational p3 p4))

;: (add rf1 rf2)




