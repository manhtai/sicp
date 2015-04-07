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

(define (matrix-*-matrix n m)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v))  m)))

(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define n '((1 0 0 0) (0 1 0 0) (0 0 1 0)))
(define i '((1 0 0) (0 1 0) (0 0 1)))
(define k '(1 1 1 1))
(define v '(1 1 1))
(define w '(7 8 9))

(dot-product v w) ; 24
(matrix-*-vector m k) ; '(10 21 30)
(transpose m) ; '((1 4 6) (2 5 7) (3 6 8) (4 6 9)) 
(matrix-*-matrix m i) ; m


;; EXERCISE 2.38
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) 
;; (/ 1 (/ 2 (/ 3 1)))
;; 3/2
(fold-left / 1 (list 1 2 3)) 
;; (/ (/ (/ 1 1) 2) 3)
;; 1/6
(fold-right list nil (list 1 2 3)) 
;; (list 1 (list 2 (list 3 nil)))

(fold-left list nil (list 1 2 3))
;; (list (list (list 1 nil) 2) 3)

;; If (op x y) = (op y x) than fold-right and fold-left are the same

;; EXERCISE 2.39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(reverse '(1 2 3 4 5))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))

(reverse '(1 2 3 4 5))


;; =========================================================================
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(prime-sum-pairs 5)


(define (permutations s)
  (if (null? s)                         ; empty set?
      (list nil)                        ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))


;; EXERCISE 2.40
(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(unique-pairs 3)

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 5)

;; EXERCISE 2.41
(define (s-sum? s t)
  (= s (+ (car t) (cadr t) (caddr t))))

(define (make-t-sum t)
  (list (car t) (cadr t) (caddr t) (+ (car t) (cadr t) (caddr t))))

(define (unique-t n)
  (flatmap
    (lambda (k)
      (map (lambda (ij) (cons k ij))
           (unique-pairs (- k 1))))
    (enumerate-interval 1 n)))

(define (find-s-sum n s)
  (map make-t-sum
       (filter (lambda (x) (s-sum? s x)) (unique-t n))))

(find-s-sum 6 12) ; '((5 4 3 12) (6 4 2 12) (6 5 1 12))

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


;; First step: Do we want to add (row column) to the rest?
(define (safe-rc? r c rest)
  (= 0 (accumulate + 0 (map (lambda (x) (if (or (= c (cadr x)) ; not in same row
                                                (= r (car x))  ; not in same colume
                                                (= 1 (abs (/ (- c (cadr x))
                                                             (- r (car x))))))
                                                               ; not in same diagonal
                                         1
                                         0))
                            rest))))

;; Second step: If we want, then add it!
(define (adjoin-position r c positions)
  (if (safe-rc? r c positions)
  (append positions (list (list r c)))
  nil))

;; Empty board for the zero step
(define empty-board nil)

;; Last step: Are we safe?
(define (safe? k rest)
  (= (- k 1) (accumulate + 0 (map (lambda (x) (if (= k (cadr x)) 0 1)) rest))))

;; If we're safe, then we're done!
(queens 4) ; 2 cases

(queens 8) ; Try for yourself!

;; EXERCISE 2.43
;; Louis's version of queens
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
           (lambda (new-row)                        ; exchange this
             (map (lambda (rest-of-queens)          ; with this
                    (adjoin-position new-row k rest-of-queens))
                  (queen-cols (- k 1))))            ; exchange this
           (enumerate-interval 1 board-size)))))    ; with this
  (queen-cols board-size))

;; This version list new n positions in column k joining the first k-1 columns
;; N TIMES WITH EACH ROW FROM 1 TO BOARD-SIZE. There are n (k-1 columns) then
;; the running time is O(n^n).

;; Exercise 2.42 version list new n positions in column k joining the first k-1 colums
;; just ONCE. There are n (k-1 colums) then the running time is O(n^2)

;; So I guess the time in the worse version is propotion to n^n/n^2. I mean n^(n-2).T

;; With n = 8 we have time 8^6 = 262144.T compare with T in the better version.

;; => Algorithms matter!
;; (But were're learning SICP, not Algorithms, by the way)

;; =========================================================================
;; A Picture Language

;; Test sicp-racket package
(open-canvas)
(draw wave)

(close-canvas)


;; Start learning!

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(clear-canvas)
(draw (right-split wave 3))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(clear-canvas)
(draw (corner-split wave 3))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


;; EXERCISE 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; For later testing
(clear-canvas)
(draw (up-split wave 3))

;; EXERCISE 2.45
(define (split dir-a dir-b)
  (lambda (p n)
    (if (= n 0)
        p
        (let ((smaller ((split dir-a dir-b) p (- n 1))))
          (dir-a p (dir-b smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

;; For later testing
(clear-canvas)
(draw (right-split wave 3))

(clear-canvas)
(draw (up-split wave 3))

(close-canvas)

;; EXERCISE 2.46

(define (make-vect a b) (cons a b))
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v) ; vec after s
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; EXERCISE 2.47
;; Book code
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; Implement 1
(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

;; ;; Implement 2
;; (define (make-frame origin edge1 edge2) (cons origin (cons edge1 edge2)))
;; (define origin-frame car)
;; (define edge1-frame cadr)
;; (define edge2-frame cddr)


;; EXERCISE 2.48
(define (make-segment v1 v2) (list v1 v2))
(define start-segment car)
(define end-segment cadr)

;; EXERCISE 2.49

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (line ; we use line here instead of draw-line to draw it in canvas
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


;; part a: outline

(define outline
  (segments->painter
    (list (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
          (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0))
          (make-segment (make-vect 1.0 1.0) (make-vect 1.0 0.0))
          (make-segment (make-vect 1.0 0.0) (make-vect 0.0 0.0)))))

(open-canvas)
(draw outline)

;; part b: X

(define X
  (segments->painter
    (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
          (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0)))))

(clear-canvas)
(draw X)

;; part c: diamond

(define diamond
  (segments->painter
    (list (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
          (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
          (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
          (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0)))))

(clear-canvas)
(draw diamond)

;; part d: wave

(clear-canvas)
(draw wave)

;; EXERCISE 2.50
;; Book code
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)    ; new origin
                     (make-vect 1.0 1.0)    ; new end of edge1
                     (make-vect 0.0 0.0)))  ; new end of edge2

;; Test that!
(clear-canvas)
(draw (flip-vert wave))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(clear-canvas)
(draw (beside wave diamond))

;; Our code
;; flip-horiz
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)    ; new origin
                     (make-vect 0.0 0.0)    ; new end of edge1
                     (make-vect 1.0 1.0)))  ; new end of edge2

(clear-canvas)
(draw (flip-horiz wave))

;; rotate 180
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(clear-canvas)
(draw (rotate180 wave))

;; rotate 270
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(clear-canvas)
(draw (rotate270 wave))


;; EXERCISE 2.51
;; Way 1
(define (below p1 p2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
           (transform-painter p1
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-bottom
           (transform-painter p2
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(clear-canvas)
(draw (below wave wave2))


;; Way 2
(define (below p1 p2)
  (rotate180 (rotate90 (beside (rotate90 p2) (rotate90 p1)))))


(clear-canvas)
(draw (below wave wave2))

;; EXERCISE 2.52
;; part a: smile Geogre

(define smile-geogre
  (segments->painter
    (append (list (make-segment (make-vect 0.40 0.80) (make-vect 0.50 0.70))
                  (make-segment (make-vect 0.50 0.70) (make-vect 0.60 0.80))
                  (make-segment (make-vect 0.35 0.85) (make-vect 0.40 0.90))
                  (make-segment (make-vect 0.40 0.90) (make-vect 0.45 0.85))
                  (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.90))
                  (make-segment (make-vect 0.60 0.90) (make-vect 0.55 0.85)))
            geogre)))

(clear-canvas)
(draw smile-geogre)


;; part b and c original book code
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(clear-canvas)
(draw (corner-split smile-geogre 3))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(clear-canvas)
(draw (square-limit smile-geogre 3))

;; part b: 
(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((right (right-split painter (- n 1)))
            (up (up-split painter (- n 1)))
            (corner (corner-split2 painter (- n 1))))
        (beside (below painter up)
                (below right corner))))) 

(clear-canvas)
(draw (corner-split2 smile-geogre 3))


;; part c:
(define (square-limit2 painter n)
  (let ((quarter (corner-split2 (flip-horiz painter) n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(clear-canvas)
(draw (square-limit2 smile-geogre 3))

(clear-canvas)
(draw (square-limit smile-geogre 3))

(close-canvas)


