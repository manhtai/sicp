;; EXERCISE 2.53

(list 'a 'b 'c) ; '(a b c)

(list (list 'george)) ; '((geogre))

(cdr '((x1 x2) (y1 y2))) ; '((y1 y2))

(cadr '((x1 x2) (y1 y2))) ; '(y1 y2)

(pair? (car '(a short list))) ; #f

(memq 'red '((red shoes) (blue socks))) ; #f

(memq 'red '(red shoes blue socks)) ; '(read shoes blue socks)


;; EXERCISE 2.54
(define (equal2? a b)
  (cond ((and (symbol? a) (symbol? b)) (eq? a b))
        ((and (pair? a) (pair? b))
         (and (equal2? (car a) (car b))
              (equal2? (cdr a) (cdr b))))
        (else #t)))

(equal2? '(this is a list) '(this is a list)) ; #t
(equal2? '(this is a list) '(this (is a) list)) ; #f

;; EXERCISE 2.55
(car ''abracadabra) ; ''abracadabra = quote + abracadabra

;; EXERCISE 2.56

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((expo? exp)
         (make-product
           (exponent exp)
           (make-expo (base exp) (- (exponent exp) 1))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (expo? x)
  (and (pair? x) (eq? (car x) '**)))
(define base cadr)
(define exponent caddr)
(define (make-expo b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) e)
        (else (list '** b e))))

(deriv '(** x 5) 'x) ; '(* 5 (** x 4))

;; EXERCISE 2.57
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(deriv '(* x y (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))

;; EXERCISE 2.58
;; a)

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;; Test that
(deriv '(x + (3 * (x + (y + 2)))) 'x) ; 4


;; b)

;; ;; I DON'T KNOW HOW TO DO IT YET,
;; ;; MARKED!
;; 
;; (define (sum? x)
;;   (and (pair? x) (eq? (cadr x) '+)))
;; 
;; (define (addend s) (car s))
;; (define (augend s) (caddr s))
;; 
;; (define (product? x)
;;   (and (pair? x) (eq? (cadr x) '*)))
;; 
;; (define (multiplier p) (car p))
;; (define (multiplicand p) (caddr p))
;; 
;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0) a2)
;;         ((=number? a2 0) a1)
;;         ((and (number? a1) (number? a2)) (+ a1 a2))
;;         (else (list a1 '+ a2))))
;; 
;; (define (make-product m1 m2)
;;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;;         ((=number? m1 1) m2)
;;         ((=number? m2 1) m1)
;;         ((and (number? m1) (number? m2)) (* m1 m2))
;;         (else (list m1 '* m2))))
;; 
;; ;; Test that
;; (deriv '(x + 3 * (x + (y + 2))) 'x) ; 4

;; EXERCISE 2.59

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2) 
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define s1 '(1 2 3 4 5))
(define s2 '(1 9 8 4 5))
(intersection-set s1 s2)
(union-set s1 s2) ; '(1 2 3 4 5 9 8)

;; EXERCISE 2.60

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; Change for adjoin-set
(define (adjoin-set x set)
      (cons x set))

;; No change for intersection-set!
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Change for union-set
(define (union-set set1 set2)
  (cond ((null? set1) set2) 
        ((null? set2) set1)
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define s1 '(1 2 3 4 5))
(define s2 '(1 9 8 4 5))
(intersection-set s1 s2) ; '(1 4 5)
(union-set s1 s2) ; '(1 2 3 4 5 9 8)

;; Maybe this is efficient in applications which don't use intersection much
;; because this procedure doesn't change at all!

;; EXERCISE 2.61

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((y (car set)))
        (cond ((= x y) set)
              ((< x y) (cons x set))
              (else (cons y (adjoin-set x (cdr set))))))))

;; This procedure requires about half as many steps as before because
;; it's coded very similar to element-of-set

(define s1 '(1 3 5 7 9))
(define s2 '(1 2 4 6))

(adjoin-set 5 s2) ; '(1 2 4 5 6)

;; EXERCISE 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1) set2)))
                  (else (cons x2 (union-set (cdr set2) set1))))))))

(union-set s1 s2) ; '(1 2 3 4 5 6 7 9)


;; EXERCISE 2.63
;; Method 1:
;; Recusive process on left branch and right branch
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

;; I don't know for sure, but this is a recursive process too
;; and I don't see the difference, I guess they're the same!
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; SOLUTION (not mine):

;; for tree->list-1, there will be 1 'append' and 1 'cons' for each node
;; cons is O(1), append is O(n), so the complexity if O(n^2)

;; for tree->list-2, there will be 1 'cons' for each node,
;; cons is O(1), so the complexity is O(n)


;; EXERCISE 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; the procedure build balance tree from left to right
;; using the form left-tree + entry + right-tree
;; first it build the left, then it takes entry, at last it build the right
;; and put them all together

;; On the list (1 3 5 7 9 11)
;; n = 6, left-size is (6 - 1) // 2 = 2
;; so left-result is build from (1 3 5 7 9 11) with size 2
;; so entry is 5, right-tree is build from (7 9 11) with size 3

;; EXERCISE 2.65
;; Union and Intersection using balanced binary trees

;; If we assume both are ordered lists, we can use union-set and intersection-set
;; for ordered lists with running time O(n), then we use tree->list-2 and list->tree
;; both runs in O(n) time to define union and intersection, each runs in O(n) time.

;; Otherwise, if the lists are not ordered we can implement an O(n log n) running
;; time sort beforehand, and total running time is now O(n log n)

;; Am I right? Yeah, I guess ;)

;; EXERCISE 2.66
;; Ordered binary tree, same as before

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))))

;; =========================================================================
;; Huffman code, I've just learn it from Algo II. Now is the time for some coding
;; since this topic isn't included in the class assignments (The professor used 2 knapsack
;; problems instead, one of which is very challenging indeed)

;; EXERCISE 2.67












