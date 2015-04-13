;; EXERCISE 2.73
;; Transform into data-directed style
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a)
;; Procedure 'deriv' check exp type and return immediately if type is number
;; or variable, otherwise it will get the proper method for that type using
;; type-tag information

;; We can't assimilate the predicates number? and variable? into the dispatch 
;; because they don't have their own type-tag 

;; b)
(define (install-sum-deriv)
  ;; internal procedures
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  ;; rename 'deriv' to another name because the deriv inside it have generic property
  (define (deriv-s exp var)
    (make-sum (deriv (addend exp) var)
               (deriv (augend exp) var)))
  ;; interface to the rest of the system
  ;; it says: deriv in '+' property is 'deriv-s'
  (put 'deriv '+ deriv-s))

(install-sum-deriv)
(deriv '(+ y (+ x x)) 'x)

(define (install-product-deriv)
  ;; internal procedures
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  ;; rename 'deriv' to another name because the deriv inside it have generic property
  (define (deriv-p exp var)
    (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
  ;; interface to the rest of the system
  ;; it says: deriv in '*' property is 'deriv-p'
  (put 'deriv '* deriv-p))

(install-product-deriv)
(deriv '(+ y (* x x)) 'x) ; (+ x x)


;; c)
(define (install-expo-deriv)
  ;; insider
  (define (base s) (car s))
  (define (exponent s) (cadr s))
  (define (make-expo b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          (else (list '** b e))))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  ;; deriv exponential
  (define (deriv-e exp var)
    (make-product
      (make-product
         (exponent exp)
         (make-expo (base exp) (- (exponent exp) 1)))
      (deriv (base exp) var)))
  ;; outsider
  (put 'deriv '** deriv-e))

(install-expo-deriv)
(deriv '(+ (** x 2) 2) 'x) ; (* x 2)
(deriv '(** (+ (** x 2) 2) 3) 'x)

;; d)
;; If we change from
;; ((get 'deriv (operator exp)) (operands exp) var)
;; to
;; ((get (operator exp) 'deriv) (operands exp) var)
;; then the derivative system have to change every 'put' operator

;; EXERCISE 2.74
;; a)
(define (get-record personnel employee)
  (apply-generic 'get-record personel employee))
;; Each individual division's files should contains its ID to differ it with
;; other division so the 'get-record' can work

;; b)
(define (get-salary personnel employee)
  (let ((record (get-record personnel employee))
    (apply-generic 'get-salary record))))

;; Each record should contain information about its division as well

;; c)
(define (find-employee-record employee personnels)
  (if (null? personnels)
      #f
      (let ((record (get-record (car personnels) employee)))
        (if record
            record
            (find-employee-record employee (cdr personnels))))))

;; d)
;; New company should include its ID in its files and records. All will be fine.

;; EXERCISE 2.75

;; Message passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;; EXERCISE 2.76

;; a) 3 strategies considered
;; explicit dispatch:
;; => to add new types:
;; - we must add to each of the generic interface procedures new types and 
;; coressponding operations
;; => to add new operations:
;; - we must add new operations to generic procedures coresspond to their type
;; - new operations must not have the same names with existing operations in the system
;;
;; data-directed style:
;; => to add new types:
;; - we must install a new package to the table with type and coressponding operations
;; => to add new operations:
;; - all packages want to use new operations must be updated
;;
;; message-passing style:
;; => to add new types:
;; - we must construct a new dispatch contains all operations inside
;; => to add new operations:
;; - we must update all data types to incorporate new operations 

;; b) In a system that new types must often be added in:
;; => we should use data-directed style because we only need to install new packages to system
;; all other types and their operations remain the same
;; => if we use message-passing style, we must change constructor inside each operations
;; for each new type arrives 

;; c) In a system that new operations must often be added in:
;; => we should use message-passing style because we only need to construct a dispatch inside
;; each new operation for all types, all types and their operations remain the same
;; => if we use data-directed style, we must update all old types to encompass each 
;; new operation


