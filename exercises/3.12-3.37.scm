;; ========================================================================== 
;; Racket has stoped using set-car! and set-cdr! so we have to use r5rs library
(require r5rs)
;; to use theses functions. Things are quite awful now, geez...
;; Tip: Use (display) to clear the awful away :D
;; ========================================================================== 

;; EXERCISE 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append  x y))
z
(cdr x)
;; '(b)
(define w (append! x y))
w
(cdr x)
;; '(b c d)


;; EXERCISE 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;; (a | ) -> (b | ) -> (c | )
;;  ^                     |
;;  |                     |
;;  +---------------------+
;; => Call z or (last-pair z)  will make an infinite loop. Don't even try!


;; EXERCISE 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; 'loop' appends y to reversed x, so 'mystery' will reverse x

(define v (list 'a 'b 'c 'd))

(define w (mystery v))

v
;; '(a)

w
;; '(d c b a)


;; EXERCISE 3.15
;; set-to-wow! set 'a to 'wow, in z1 'a is pointed from one object so two 'a changed
;; in z1 'a is pointed from 2 diffirent objects so just one '1 changed.


;; EXERCISE 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define a3 (cons 'a (cons 'b (cons 'c '()))))
(count-pairs a3) ; 3

(define x (cons 'a '()))
(define a4 (cons x (cons x '())))
(count-pairs a4) ; 4

(define x (cons 'a '()))
(define y (cons x x))
(define a7 (cons y y))
(count-pairs a7) ; 7

(count-pairs a!) ; ! (make-cycle) in Exercise 3.13

;; I STILL DON'T KNOW WHAT "MADE UP OF EXACTLY 3 PAIRS" MEAN, BUT INDEED
;; THERE IS 3, 4, AND 7 PAIRS IN THOSES LIST IF WE COUNT AS BEN COUNT

;; EXERCISE 3.17
(define (count-pairs x)
  (let ((encountered '()))
    (define (helper x)
      (if (or (not (pair? x)) (memq x encountered))
          0
          (begin
            (set! encountered (cons x encountered))
            (+ (helper (car x))
               (helper (cdr x))
               1))))
    (helper x)))

(count-pairs a3) ; 3
(count-pairs a4) ; 3
(count-pairs a7) ; 3


;; I STILL DON'T KNOW WHAT "MADE UP OF EXACTLY 3 PAIRS" MEAN, BUT IF WE USE
;; 'memq' THEN IT CHECK FOR POINTER AND THEN WE HAVE 3 PAIRS EACH

;; EXERCISE 3.18
;; Check for cycle

(define (cycle? c)
  (define visited nil)
  (define (iter x)
    (set! visited (cons x visited))
    (cond ((null? (cdr x)) #f)
          ((memq (cdr x) visited) #t)
          (else (iter (cdr x)))))
  (iter c))

(define z (make-cycle (list 'a 'b 'c)))

(cycle? z) ; #t


;; EXERCISE 3.19
;; http://en.wikipedia.org/wiki/Cycle_detection
;; I haven't learned about this yet, so I leave it here as an open challenge for me
(define (cycle? c))

(cycle? z) ; #t

;; ==========================================================================  
;; QUEUE IMLEMENTATION
;; ==========================================================================  

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 


;; EXERCISE 3.21
(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)

(define (print-queue q)
  (display (car q)))

;; Redo it
(define q1 (make-queue))
(print-queue q1)
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)

;; EXERCISE 3.22
;; Queue as procedure
(define (make-queue)
  (let ((front-ptr car)
        (rear-ptr cdr)
        (queue (cons '() '())))
    ;; Internal procedures
    (define (empty-queue?) (null? (front-ptr queue)))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" queue)
          (car (front-ptr queue))))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-car! queue new-pair)
               (set-cdr! queue new-pair))
              (else
               (set-cdr! (rear-ptr queue) new-pair)
               (set-cdr! queue new-pair))))) 
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" queue))
            (else
             (set-car! queue (cdr (front-ptr queue)))))) 
     ;; Procedures to the outside world
     (define (dispatch m)
       (cond ((eq? m 'empty-queue?) (empty-queue?))
             ((eq? m 'front-queue) (front-queue))
             ((eq? m 'insert-queue!) insert-queue!)
             ((eq? m 'delete-queue!) (delete-queue!))
             ((eq? m 'print-queue) (display (car queue)))))
      dispatch))

;; Test it
(define q1 (make-queue))
(q1 'print-queue)
((q1 'insert-queue!) 'a)
(q1 'print-queue)
((q1 'insert-queue!) 'b)
(q1 'print-queue)
(q1 'delete-queue!)
(q1 'print-queue)
(q1 'delete-queue!)
(q1 'print-queue)

;; EXERCISE 3.23:
;; De-queue as procedure
;; I leave it here as an open challenge

;; Test it
(define q2 (make-dequeue))
(q2 'print-queue) ; ()

((q2 'front-insert-queue!) 'b)
((q2 'front-insert-queue!) 'a)
((q2 'rear-insert-queue!) 'c)
(q2 'print-queue) ; (a b c)

;; ==========================================================================  
;; TABLE IMLEMENTATION
;; ==========================================================================  

;; EXERCISE 3.24

(define (make-table same-key?)
  (let ((table (list '*table*)))
    ;; Internal
    (define (lookup key)
      (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            false)))

    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table
                      (cons (cons key value) (cdr table)))))
      'ok)
    ;; Outer
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)))
    dispatch))

(define (same-key? a b)
  (< (abs (- a b)) 0.01))

(define t (make-table same-key?))

((t 'insert!) 10 20)
((t 'lookup) 10.01) ; 20

;; EXERCISE 3.25
;; The 'key' variable in Exercise 3.24 could be a list. So are we done?

(define t (make-table equal?))
((t 'insert!) '(letter a) 97)
((t 'insert!) '(letter b) 98)
((t 'insert!) '(math +) 43)
((t 'insert!) '(math -) 45)


((t 'lookup) '(letter a))
((t 'lookup) '(letter b))
((t 'lookup) '(math +))

;; multiple keys
((t 'insert!) '(math complex div) 100)
((t 'lookup) '(math complex div))

;; one key
((t 'insert!) '(a) 1)
((t 'lookup) '(a))

;; EXERCISE 3.26
;; Table using binary tree
(define (make-table)
  (let ((table (list '*table*)))
    ;; For multiple keys comparation
    ;; I leave it as an open challenge :))
    (define =? =)
    (define >? >)
    (define <? <)
    ;; Internal procedure
    (define (lookup key)
      (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            false)))

    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table
                      (adjoin-set key value (cdr table))))))
        
    ;; Tree implementation as pretty much the same as in chapter 2
    (define (entry tree) (car tree))
    (define (left-branch tree) (cadr tree))
    (define (right-branch tree) (caddr tree))
    (define (make-tree entry left right)
      (list entry left right))
    (define (adjoin-set key value set)
      (cond ((null? set) (make-tree (cons key value) '() '()))
            ((=? key (car (entry set))) set)
            ((<? key (car (entry set)))
             (make-tree (entry set) 
                        (adjoin-set key value (left-branch set))
                        (right-branch set)))
            ((>? key (car (entry set)))
             (make-tree (entry set)
                        (left-branch set)
                        (adjoin-set key value (right-branch set))))))
    (define (assoc given-key set-of-records)
      (cond ((null? set-of-records) #f)
            ((=? given-key (car (entry set-of-records)))
             (entry set-of-records))
            ((>? given-key (car (entry set-of-records)))
             (assoc given-key (right-branch set-of-records)))
            ((<? given-key (car (entry set-of-records)))
             (assoc given-key (left-branch set-of-records)))))

    ;; Outer interface
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)))
    dispatch))

;; Test that!
(define t (make-table))
((t 'insert!) 1  97)
((t 'insert!) 1  98)
((t 'insert!) 2  108)

((t 'lookup) 1)
((t 'lookup) 2)

;; EXERCISE 3.27
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result ((table 'lookup) x)))
        (or previously-computed-result ;; 'or' new use :D
            (let ((result (f x)))
              ((table 'insert!) x result)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(memo-fib 3)

;; 'memo-fib' helps to turn an tree recursion into a linear recursion
;; further calls to memo-fib will lookup previous results
;; the table which is created by 'memorize' in the clojure of a lambda
;; so the storage is persistent

;; (memoize fib) won't have the same effect because in the recursion
;; body of 'fib', 'fib' is not memorized



;; ==========================================================================  
;; DIGITAL CIRCUITS SIMULATOR
;; I leave it here a an open c 8-)
;; Because I need to learn about circuit before doing exercise though I can
;; do it blindly :D
;; ==========================================================================  


;; ==========================================================================  
;; PROPAGATION OF CONSTRAINTS
;; ==========================================================================  
;; Book code
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints 
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


;; EXERCISE 3.33
(define (averager a b c)
  (let ((u (make-connector))
        (w (make-connector)))
    (multiplier c w u)
    (adder a b u)
    (constant 2 w)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(probe "a" a)
(probe "b" b)
(probe "c" c)
(averager a b c)

(set-value! a 10 'user)
(set-value! b 30 'user)

(forget-value! b 'user)
(set-value! c 40 'user)

;; EXERCISE 3.34

(define (squarer a b)
  (multiplier a a b))

(define a (make-connector))
(define b (make-connector))
(probe "a" a)
(probe "b" b)
(squarer a b)

(set-value! a 10 'user) ;; Probe b = 100, correct!

(forget-value! a 'user)
(set-value! b 100 'user) ;; Probe b = 100, we've already known that!
;; => Set value for b doesn't trigger value change for a because 'multiplier'
;; need 2 input for calculate the third.
;; Actually we need an additional constraint namely 'sqrt' to do this since
;; we can't get a from b using only + and *, and it's next exercise

;; EXERCISE 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (set-value! b (square (get-value a)) me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define a (make-connector))
(define b (make-connector))
(probe "a" a)
(probe "b" b)
(squarer a b)

(set-value! b 100 'user) ;; Probe a = 10, correct!

;; EXERCISE 3.36
;: (define a (make-connector))
;: (define b (make-connector))
;: (set-value! a 10 'user)


;; EXERCISE 3.37
;; Primitives in new styles
;; c+
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
;; c*
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
;; cv
(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))
;; c/
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

;; Old style converter
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user) ;; 77

;; New style converter
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user) ;; 77?


