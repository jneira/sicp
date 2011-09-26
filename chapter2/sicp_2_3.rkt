;; 2.3  Symbolic Data

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; Exercise 2.53.

(list 'a 'b 'c)
; (a b c)
(list (list 'george))
;((george))
(cdr '((x1 x2) (y1 y2)))
;((y1 y2)) 
(cadr '((x1 x2) (y1 y2)))
;;(y1 y2)
(pair? (car '(a short list)))
;;false
(memq 'red '((red shoes) (blue socks)))
;;false
(memq 'red '(red shoes blue socks))
;;(red shoes blue socks)

;;Exercise 2.54.

(define (equal? xs ys)
  (let* ((hx (car xs)) (hy (car ys))
         (tx (cdr xs)) (ty (cdr y)))
    (or
     (and (null? xs) (null ys))
     (and (eq? hx hy)
          (equal? tx ty))
     (and (pair? hx) (pair? hy)
          (equal? hx hy)
          (equal? tx ty)))))

;;Exercise 2.55.

(car ''abracadabra)
;; (car (quote (quote abracadabra)))
;; (car (list 'quote 'abracadabra))

;; 2.3.2  Example: Symbolic Differentiation

(define (variable? x) (symbol? x))
(define num? number?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((num? exp) 0)
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
        (else
         (error "unknown expression type -- DERIV" exp))))


(deriv '(+ x 3) 'x)
;;(+ 1 0)
(deriv '(* x y) 'x)
;;(+ (* x 0) (* 1 y))
(deriv '(* (* x y) (+ x 3)) 'x)
;;(+ (* (* x y) (+ 1 0))
;;   (* (+ (* x 0) (* 1 y))
;;      (+  x 3)))

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

(deriv '(+ x 3) 'x)
;; 1
(deriv '(* x y) 'x)
;; y
(deriv '(* (* x y) (+ x 3)) 'x)
;; (+ (* x y) (* y (+ x 3)))

;; Exercise 2.56

(define (exponentiation? form)
  (and (pair? form) (eq? (car form) '**)))

(define base cadr)
(define exponent caddr)

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list '** base exp))))

(define (deriv exp var)
  (cond ((num? exp) 0)
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
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp)
                         (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; Exercise 2.57

(define (augend form)
  (if (> (length form) 3)
      (cons '+ (cddr form))
      (caddr form)))

(define (multiplicand form)
  (if (> (length form) 3)
      (cons '* (cddr form))
      (caddr form)))

(deriv '(* x y (+ x 3)) 'x)

;; Exercise 2.58
;; a

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define addend car)
(define augend caddr)
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define multiplier car)
(define multiplicand caddr)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list  a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
;; 4

;; b

(define (singleton x)
  (if (and (pair? x) (= 1 (length x)))
      (car x) x))

(define (sum? form)
  (and (pair? form)
       (member '+ form)))

(define (addend form)
  (singleton
   (drop-right
    form (+ 1 (if (pair? (augend form))
                  (length (augend form)) 1)))))

(define (augend form)
  (singleton (cdr (member '+ form))))

(define (product? form)
  (and (pair? form)
       (not (sum? form))
       (eq? (cadr form) '*)))

(define multiplier (compose singleton car))
(define multiplicand (compose singleton cddr))

;; (x + 3 * (x + y + 2)) --> (x + (3 * (x + y + 2)))
;; (x + 3 + y) --> (x + (3 + y))
;; (3 * (x + y + 2) + x) -->  ((3 * (x + y + 2)) + x)
;; (3 * y * x) --> ((3 * y) * x)

(deriv '(x + 3 * (x + y + 2)) 'x)
;; 4
(deriv '(x + 3 * x + y + 2) 'x)
;; 4

;; 2.3.3  Example: Representing Sets

;; Sets as unordered lists

(define (ul/element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (ul/adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (ul/intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Exercise 2.59.

(define (ul/union-set s1 s2)
  (cond
   ((null? s1) s2)
   ((null? s2) s1)
   (else 
    (union-set (adjoin-set (car s2) s1) (cdr s2)))))

;; Exercise 2.60

(define ul/adjoin-set cons)

;; Insertion becomes easier but rest of procs which have to recurse
;; through the set will be slower (they have to go through duplicate
;; elements) but the order of growth remains the same O(n)
;; Only useful in apps doing much more insertions than other ops

;; Sets as ordered lists

(define (ol/element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (ol/intersection-set set1 set2)
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

;; Exercise 2.61

(define (ol/adjoin-set x set)
  (cond
   ((null? set) '())
   ((= (car set) x) set)
   ((> (car set) x) (cons x set))
   (else (cons (car set) (adjoin-set x (cdr set))))))

;; Exercise 2.62

(define (ol/union-set s1 s2)
  (cond
   ((null? s1) s2)
   ((null? s2) s1)
   (else
    (let ((h1 (car s1)) (t1 (cdr s1))
          (h2 (car s2)) (t2 (cdr s2)))
      (cond
       ((> h1 h2) (cons h2 (union-set s1 t2)))
       ((< h1 h2) (cons h1 (union-set t1 s2)))
       (else (cons h1 (union-set t1 t2))))))))

;; Sets as binary trees

(define (bt/entry tree) (car tree))
(define (bt/left-branch tree) (cadr tree))
(define (bt/right-branch tree) (caddr tree))
(define (bt/make-tree entry left right)
  (list entry left right))

(define (bt/element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (bt/adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; Exercise 2.63

(define (tree->list-1 tree)
  (write tree) (newline)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (write tree) (newline)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define tree->list tree->list-1)

;; a: one list is the reverse of the other

(tree->list-1 '(7 (3 (1 () ()) (5 () ()))
                  (9 () (11 () ()))))

(tree->list-2 '(7 (3 (1 () ()) (5 () ()))
                  (9 () (11 () ()))))

(tree->list-1 '(3 (1 () ())
                  (7 (5 () ())
                     (9 ()
                        (11 () ())))))
(tree->list-2 '(3 (1 () ())
                  (7 (5 () ())
                     (9 ()
                        (11 () ())))))

(tree->list-1 '(5 (3 (1 () ()) ())
                  (9 (7 () ()) (11 () ()))))

(tree->list-2 '(5 (3 (1 () ()) ())
                  (9 (7 () ()) (11 () ()))))

;; b. The growth is the same

;; Exercise 2.64

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

;; El algoritmo va acumulando el arbol resultante en el primer
;; elemento de la lista y se mantiene la lista de elementos a meter en
;; el arbol en ella a partir del 2. De forma recursiva (y deep-first)
;; se va construyendo el arbol usando n=0 como caso base en el que se
;; inicializa cada una de las ramas. En cada paso se divide el
;; indice entre 2 y se crea una rama izquierda y derecha con ese nuevo
;; indice 

;; better with let*

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1)))
             (right-result (partial-tree (cdr non-left-elts)
                                         right-size))
             (right-tree (car right-result))
             (this-entry (car non-left-elts))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))

;; (1 3 5 7 9 11)
;; ((4 (1 () (3 () ())) (7 (6 () ()) (8 () ()))))

;; 2.64. b.
;; O(n)=n log n

;; Exercise 2.65.

(define (bt/union-set s1 s2)
  (list->tree (ol/union-set (tree->list s1)
                            (tree->list s2))))

(define (bt/intersection-set s1 s2)
  (list->tree (ol/intersection-set (tree->list s1)
                                   (tree->list s2))))

;; Sets and information retrieval

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;; Exercise 2.66.

(define (lookup gkey records)
  (let* ((record (car records))
         (rkey (key record)))
    (cond ((null? records) null)
         ((= gkey rkey) record)
         ((<  gkey rkey)
          (lookup gkey (cadr set)))
         ((> gkey rkey)
          (lookup gkey (caddr set))))))

;; 2.3.4  Example: Huffman Encoding Trees

;; Representing Huffman trees

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

;; The decoding procedure

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

;; Sets of weighted elements

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;; Exercise 2.67.

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;;  (A D A B B C A)

;; Exercise 2.68.

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; Given a Huffman tree, we can find the encoding of any symbol by
;; starting at the root and moving down until we reach the leaf that
;; holds the symbol. Each time we move down a left branch we add a 0
;; to the code, and each time we move down a right branch we add a
;; 1. (We decide which branch to follow by testing to see which branch
;; either is the leaf node for the symbol or contains the symbol in
;; its set.)

(define (encode-symbol sym tree)
  (define (move branch bit)
    (if (member sym (symbols branch))
      (if (leaf? branch) (list bit)
        (cons bit (encode-symbol sym branch))) #f))
  (or (move (left-branch tree) 0)
      (move (right-branch tree) 1)
      (error "No symbol in tree")))

(encode '(A D A B B C A) sample-tree)
;; (0 1 1 0 0 1 0 1 0 1 1 1 0)

;; Exercise 2.69.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; Now find two leaves with the lowest weights and merge them to
;; produce a node that has these two nodes as its left and right
;; branches. The weight of the new node is the sum of the two
;; weights. Remove the two leaves from the original set and replace
;; them by this new node. Now continue this process. At each step,
;; merge two nodes with the smallest weights, removing them from the
;; set and replacing them with a node that has these two as its left
;; and right branches. The process stops when there is only one node
;; left, which is the root of the entire tree


(define (successive-merge tree)
  (if (null? (cdr tree)) (car tree)
      (successive-merge
       (adjoin-set (make-code-tree
                    (car tree) (cadr tree)) (cddr tree)))))

(define init-leaf-set
  (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))

(define test-tree (generate-huffman-tree
              '((A 4) (B 2) (C 1) (D 1))))

(equal? sample-tree test-tree)
;; #t
(encode '(A D A B B C A) test-tree)

;; Exercise 2.70.

(define fifties-tree (generate-huffman-tree
                      '((A 2) (BOOM 1) (GET 2) (JOB 2)
                        (NA 16) (SHA 3) (YIP 9) (WAH 1))))

(define song
  '(GET A JOB
         SHA NA NA NA NA NA NA NA NA
         GET A JOB
         SHA NA NA NA NA NA NA NA NA
         WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
         SHA BOOM))

(define song-encoded
  (encode song fifties-tree))

(length song-encoded)
;; 84

;; 3 bit to encode 8 symbols
(* 3 (length song))
;; 108

;; Exercise 2.71

(define n5 '((A 1) (B 2) (C 4) (D 8) (E 16)))
(define n10 '((A 1) (B 2) (C 4) (D 8) (E 16)
              (F 32) (G 64) (H 128) (I 256) (J 512)))

(generate-huffman-tree n5)
(generate-huffman-tree n10)
;; most frequent: 1 bit, least-frequent: n-1

;; Exercise 2.72.

;; n * (2n-1) || n * (n+1) ~= n^2
