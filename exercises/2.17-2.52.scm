(include "../resources/book-code/ch2support.scm")

;; EXERCISE 2.17
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

(last-pair (list 23 72 149 34)) ; 34

;; EXERCISE 2.18

(define (reverse l)
  (if (null? (cdr l))
      l
      (append (reverse (cdr l)) (list (car l)))))

(reverse (list 1 4 9 16 25))

;; EXERCISE 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))


(define (first-denomination l)
  (car l))

(define (except-first-denomination l)
  (cdr l))

(define (no-more? l)
  (null? l))

(cc 100 us-coins) ; 292


;; EXERCISE 2.20

(define (same-parity first . last)
  (let ((sign (modulo first 2)))
    (define (same last)
      (if (null? (cdr last))
          last
          (append (list (if (= (modulo (car last) 2) sign) (car last) nil))
                  (same (cdr last)))))
    (append (list first) (same last))))

;; We can do this better when we learn about `filter`, but that's another story

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)

;; EXERCISE 2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(define (square-list items)
  (map square items))

(square-list (list 1 2 3 4))

;; map is more elegant despite everything is the same inside ;)

;; EXERCISE 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

;; Order is reversed because 'car' extract from left to right and 
;; 'cons' append in that same order to answer, hence the result is
;; reversed

(square-list (list 1 2 3 4))

;; Interchange thing
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list (list 1 2 3 4))

;; This doesn't work because answer is a list, 'cons' a list to a
;; number will make list in list, not the thing we want, a list


;; EXERCISE 2.23
(define (for-each p l)
   (if (null? (cdr l))
       (p (car l))
       (for-each p ((lambda (l) (p (car l)) (cdr l)) l))))

;; Now we see that lambda body can have more than 2 combinations
;; and it's only count the last to be the return? (I'm not very sure)

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))


;; EXERCISE 2.24
;; (list 1 (list 2 (list 3 4)))
;; .
;; 1 .
;;   2 .
;;     3 4


;; EXERCISE 2.25
(define l1 '( 1 3 (5 7) 9))
(define l2 '((7)))
(define l3 '( 1 ( 2 ( 3 ( 4 ( 5 ( 6 7)))))))

(car (cdr (car (cdr (cdr l1))))) ; or shortcut
(car (cdaddr l1))

(car (car l2)) ; or shortcut
(caar l2)

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))))) ; or short cut
(cadadr (cadadr (cadadr l3)))

;; I must confess: the shortcuts I look in the solutions :D
;; I must miss things on the book

;; EXERCISE 2.26
(define x '(1 2 3))
(define y '(4 5 6))

(append x y) ; append to list: (1 2 3 4 5 6)
(cons x y) ; cons this list to begining of another list: ((1 2 3) 4 5 6)
(list x y) ; cons 2 lists together: ((1 2 3) (4 5 6))

;; EXERCISE 2.27
(define (reverse l)
  (if (null? (cdr l))
      l
      (append (reverse (cdr l)) (list (car l)))))

(define (deep-reverse x)
  (cond ((not (pair? x)) x)
        ((null? (cdr x)) (deep-reverse x)) ; If you use (deep-reverse (car x))
        (else (list (deep-reverse (cadr x)) ; then here you use (car x) here, respectly
                   (deep-reverse (car x))))))


(define x (list (list 1 2) (list 3 4)))
x
(reverse x)
(deep-reverse x)

;; EXERCISE 2.28

(define (fringe x)
  (cond ((not (pair? x)) (list x))
        ((null? (cdr x)) (fringe (car x)))
        (else (append (fringe (car x)) (fringe (cdr x))))))

(define x (list (list 1 2) (list 3 4)))
(fringe x) ; (1 2 3 4)
(fringe (list x x)) ; (1 2 3 4 1 2 3 4)


;; EXERCISE 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; part a

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cadr b))

;; 0-------1---+(7)
;; |       |
;; |       +(9)
;; |
;; 2---+(3)
;; |
;; +(4)


(define b7 (make-branch 3 7))
(define b9 (make-branch 1 9))
(define m1 (make-mobile b7 b9))
(define b1 (make-branch 8 m1))

(define b3 (make-branch 2 3))
(define b4 (make-branch 1 4))
(define m2 (make-mobile b3 b4))
(define b2 (make-branch 3 m2))

(define m0 (make-mobile b1 b2))

;; part b

(define (total-weight m)
  (define (weight b)
    (if (pair? (branch-structure b)) 
        (total-weight (branch-structure b))
        (branch-structure b)))
  (+ (weight (left-branch m))
     (weight (right-branch m))))


(total-weight m1) ; 9+7 = 16
(total-weight m2) ; 3+4 = 7
(total-weight m0) ; 7+9+3+4 = 23

;; part c

(define (balance? m)
  (let ((lb (left-branch m))
        (rb (right-branch m)))
    (define (torque b)
      (let ((bs (branch-structure b)))
        (* (branch-length b) (if (pair? bs)
                                 (total-weight bs)
                                 bs))))
    (and (= (torque lb) (torque rb))
         (if (pair? (branch-structure lb))
             (balance? (branch-structure lb))
             #t)
         (if (pair? (branch-structure rb))
             (balance? (branch-structure rb))
             #t))))

(balance? m0) ; #f

;; a---(1)
;; |
;;(3)

(define b13 (make-branch 3 1))
(define b23 (make-branch 1 3))
(define ma (make-mobile b13 b23))

(balance? ma) ; #t

;; x-----y-----y1(2)
;; |     |
;; z(35) |
;;     y2(5) 

(define y1 (make-branch 5 2))
(define y2 (make-branch 2 5))
(define my (make-mobile y1 y2))
(define y (make-branch 5 my))

(define z (make-branch 1 35))

(define x (make-mobile z y))


(total-weight x) ; 42
(balance? x) ; #t


;; part d

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; I guess I need to change only this
(define (left-branch m)
  (car m))

(define (right-branch m)
  (cdr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cdr b))

;; Verify that!

(define y1 (make-branch 5 2))
(define y2 (make-branch 2 5))
(define my (make-mobile y1 y2))
(define y (make-branch 5 my))

(define z (make-branch 1 35))

(define x (make-mobile z y))


(total-weight x) ; 42
(balance? x) ; #t

;; INDEED!


;; EXERCISE 2.30

;; Mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))


(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

;; Using map
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;; Square over tree using map
(define (square x) (* x x))
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;; Directly
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))


;; EXERCISE 2.31
(define (tree-map p tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map p sub-tree)
             (p sub-tree)))
       tree))

;; Bookmarked!

(define (square-tree tree) (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))


;; EXERCISE 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; It works because assume we had the subsets of (cdr s) then all we need
;; include to this subsets for making the set of all subsets of s
;; are lists include (car s) and the subsets of (cdr s), and the procedure
;; (lambda (x) (cons (car s) x)) take each element in rest as an argument 
;; and return the lists we need.

(subsets '(1 2 3))

;; =========================================================================
;; Sequence operations

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))


;; EXERCISE 2.33
;; some more basis list-manipulation

(define (new-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
;; Re-define map, awesome!

(new-map square '(1 2 3 4 5)) ; '(1 4 9 16 25) 

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
;; Re-define append, good!

(append '(1 2 3) '(4 5 6)) ; '(1 2 3 4 5 6)

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
;; Re-define length, fine!

(length '(1 2 3 4 5 6)) ; 6

;; EXERCISE 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1)) ; 79

;; EXERCISE 2.35
;; Old count-leaves
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x) ; 4
(count-leaves (list x x)) ; 8

;; New count-leaves

(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y)) 
              0 
              (map (lambda (x) (if (pair? x)
                                   (count-leaves x)
                                   1))
                   t)))
;; The `map` function change t to a sequence of 1 as equivalent to one leaf, 
;; the remaining job is to sum all together by `lambda`
;; Yes, map over tree it is ;)

(count-leaves x) ; 4
(count-leaves (list x x)) ; 8

;; EXERCISE 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate-n + 0 s) ; (22 26 30)

;; EXERCISE 2.37
;; Test map with default implementation
(map + (list 1 2 3) (list 40 50 60) (list 700 800 900)) ; (741 852 963)
(map (lambda (x y) (+ x (* 2 y)))
     (list 1 2 3)
     (list 4 5 6)) ; (9 12 15)

;; Matrix algebra, yeah ;))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map  m)))

(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define n '((1 0 0 0) (0 1 0 0) (0 0 1 0)))
(define i '((1 0 0) (0 1 0) (0 0 1)))
(define k '(1 1 1 1))
(define v '(1 1 1))
(define w '(7 8 9))

(dot-product v w) ; 24
(matrix-*-vector m k) ; '(10 21 30)
(transpose m)
(matrix-*-matrix m i) ; m





;; EXERCISE 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;: (fold-right / 1 (list 1 2 3))
;: (fold-left / 1 (list 1 2 3))
;: (fold-right list nil (list 1 2 3))
;: (fold-left list nil (list 1 2 3))




;; EXERCISE 2.39

;; EXERCISE 2.40


;; EXERCISE 2.41






;; =========================================================================
;; EXERCISE 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; EXERCISE 2.43
;; Louis's version of queens
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
	 ;; next expression changed
         (flatmap
	  (lambda (new-row)
	    (map (lambda (rest-of-queens)
		   (adjoin-position new-row k rest-of-queens))
		 (queen-cols (- k 1))))
	  (enumerate-interval 1 board-size)))))
  (queen-cols board-size))





