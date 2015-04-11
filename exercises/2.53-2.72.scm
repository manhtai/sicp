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

;; Am I right? We'll see
;; Evaluate
(list->tree '(1 5 3 8 2 4)) ; return
'(3 (1 () (5 () ())) (2 (8 () ()) (4 () ())))
; which isn't a binary tree, but a balanced tree, indeed.

;; EXERCISE 2.66
;; Ordered binary tree, same as before we can look in the right or in the left

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


;; Huffman code
;; representing 
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; sets
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; EXERCISE 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ; '(A D A B B C A)

;; EXERCISE 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; Recursive in left/ right
(define (encode-symbol s t)
  (let ((left (left-branch t))
        (right (right-branch t)))
    (cond ((and (leaf? left) (eq? s (symbol-leaf left))) '(0))
          ((and (leaf? right) (eq? s (symbol-leaf right))) '(1))
          ((memq s (symbols left)) (cons 0 (encode-symbol s left)))
          ((memq s (symbols right)) (cons 1 (encode-symbol s right)))
          (else (error "bad symbol: not found in tree")))))


(define m '(A D A B B C A))
(encode-symbol 'B sample-tree) ; '(1 0)
(encode m sample-tree) ; '(0 1 1 0 0 1 0 1 0 1 1 1 0)

;; EXERCISE 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; We need calculate weight for sorting below
(define (weight-of x)
  (cond ((leaf? x) (weight-leaf x))
        (else (weight x))))

;; We need to sort set after each mergings 
;; It's very similar to adjoint-set! Amazingly!
(define (merge-set tree set)
  (if (null? set)
      (list tree)
      (let ((first-weight (weight-of (car set)))
            (tree-weight (weight tree)))
        (cond ((< first-weight tree-weight) (cons (car set) (merge-set tree (cdr set))))
              (else (cons tree set))))))

;; Now we merge until only 1 last
(define (successive-merge leaf-set)
  (define (fail-merge set n)
    (if (= n 1)
        set
        (fail-merge (merge-set (make-code-tree (car set) (cadr set))
                               (cddr set)) (- n 1))))
  ; We use list of tree and leaf, so we 'car' it to get the very last tree
  (car (fail-merge leaf-set (length leaf-set))))

;; Test this using pairs in 2.70
(define song-pairs
  '((A 2) (GET 2) (SHA 3) (WAH 1)
    (BOOM 1) (JOB 2) (NA 16) (YIP 9)))

(define ls (make-leaf-set song-pairs))
(successive-merge ls)

(define song-tree (generate-huffman-tree song-pairs))

(symbols song-tree) ; '(NA YIP BOOM WAH JOB SHA GET A)
(weight song-tree) ; 36

;; EXERCISE 2.70:

;; Rock on!
(define rock '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(define rock-en (encode rock song-tree))

(length rock-en) ; 84 bits required for encoding
;; If we use 8 symbol alphabet, let's see:
(length rock) ; 35 space, 36x2 = 72 chacters at least, so we need 8x(35+72) = 856 at least


;; EXERCISE 2.71:
;; For n = 5
(define pair-5 '((A 1) (B 2) (C 4) (D 8) (E 16)))
(define tree-5 (generate-huffman-tree pair-5))
tree-5 ; 1 bit for E, 4 bits for A

;; With n in general:
;; 1 for most frequent symbol
;; n-1 for least frequent, I guess, hehe

;; EXERCISE 2.72:
;; My encode function use 'memq' which runs in O(n) so for most frequent symbol
;; it required O(1) but for least frequent it require O(n^2)






