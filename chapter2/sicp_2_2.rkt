;; 2  Building Abstractions with Data

;; 2.2  Hierarchical Data and the Closure Property

;; 2.2.3  Sequences as Conventional Interfaces

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

;; this is the fringe (or flatten) proc
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;; Exercise 2.33.

(define (map1 p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) null sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ y 1) ) 0 sequence))

;; Exercise 2.34.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
               (+ this-coeff (* x higher-terms)) )
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;; Exercise 2.35.

(define (count-leaves t)
  (accumulate
   (lambda (x y) (+ x y)) 0
   (map (lambda (x)
          (if (list? x)
              (count-leaves x) 1)) t)))

;; Exercise 2.36.

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate-n + 0 s)

;; Exercise 2.37.

(define (dot-product v w) 
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x)
         (dot-product v x))  m))
(define (transpose mat)
  (accumulate-n
   (lambda (x y) (cons x y))
   null mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (matrix-*-vector cols x)) m)))

;; Exercise 2.38.

(define fold-right accumulate)

;; accumulate/fold-right (op x1 (op x2 .. (op xn initial) .. ))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; fold-left (op (op .. (op initial x1) ..  x2) xn )

(fold-right / 1 (list 1 2 3))
;; 3/2
(fold-left / 1 (list 1 2 3))
;; 1/6
(fold-right list null (list 1 2 3))
;;  (1 (2 (3 ())))
(fold-left list null (list 1 2 3))
;; (((() 1) 2) 3)

;; the commutative property

;; Exercise 2.39.

(define (reverse sequence)
  (fold-right (lambda (x y)(append y (list x))) null sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

;; Nested Mappings

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list null)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

;; Exercise 2.40.

(define (unique-pairs from to )
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval from (- i 1))))
   (enumerate-interval from to)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs 1 n))))

;; Exercise 2.41.

(define (ordered-triples from to)
  (flatmap
   (lambda (i)
     (map (lambda (j) (cons i j))
          (unique-pairs from (- i 1))))
     (enumerate-interval from to)))
                  

(define (triples-up-to-n-equals-to-x n x)
  (filter (lambda (triple)
            (= x (apply + triple)))
          (ordered-triples 1 n)))

;; Exercise 2.42.

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
            (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position row col queens)
  (cons (list row col) queens))
(define empty-board null)

(define row car)
(define col cadr)

(define (some-in-same-row cell cells-with-queens) 
  (let ((rows (map row cells-with-queens)))
    (member (row cell) rows)))

(define (some-in-same-col cell cells-with-queens) 
  (let ((cols (map col cells-with-queens)))
    (member (col cell) cols)))

(define (writenl x) (write x) (newline))

(define (in-same-diagonal cell1 cell2)
  (and (= (abs (- (first cell1) (first cell2)))
          (abs (- (second cell1) (second cell2))))
       (not (= (first cell1) (first cell2)))))

(in-same-diagonal '(1 1) '(2 2))

(define (some-in-same-diagonal cell queens)
  (ormap (lambda (queen) (in-same-diagonal cell queen)) queens))

(define (safe? k queens) 
  (andmap (lambda (f) (not (f (car queens) (cdr queens))))
          (list some-in-same-row
                some-in-same-col
                some-in-same-diagonal)))

(define (queens2 board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

; the second version do one recursive call for each interval step
; the complexity is N^board-size

;; 2.2.4  Example: A Picture Language

;; Exercise 2.45.  Right-split and up-split can be expressed as
;; instances of a general splitting operation. Define a procedure
;; split with the property that evaluating

;;(define right-split (split beside below))
;;(define up-split (split below beside))

;; produces procedures right-split and up-split with the same
;; behaviors as the ones already defined.


(define (split fst snd)
  (define (splitter painter n)
   (if (= n 0)
       painter
       (let ((smaller (splitter painter (- n 1))))
         (fst painter (snd smaller smaller)))))
  splitter)
(define beside null)
(define below null)
(define right-split (split beside below))
(define up-split (split below beside))

;; Exercise 2.46.

(define make-vect list)
(define xcor-vect car)
(define ycor-vect cadr)

(define add-vect (curry map +))
(define sub-vect (curry map -))
(define scale-vect (curry map (lambda (x) (* x s))))

;; Exerecise 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame first)
(define edge1-frame second)
(define edge2-frame third)

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)

;; Exercise 2.48.

(define make-segment list)
(define start-segment first)
(define end-segment second)

;; Exercise 2.49

;;a

(define (outline-segments corners) 
  (let* ((vectors-orig
          (map (curry apply make-vect) corners))
         (vectors-dest
          (append (cdr vectors-orig)
                  (list (car vectors-orig)))))
    (map make-segment
         vectors-orig vectors-dest)))

(define (segment->painter segment) null)

(define outline
  (segment->painter
   (outline-segments '((0 0) (0 1) (1 1) (1 0)))))

;;b
(define x-segments
  (map make-segment
       (list (make-vector 0 0)
             (make-vector 0 1))
       (list (make-vector 1 1)
             (make-vector 1 0))))

(define x (segment->painter x-segments))

;;c
(define diamond-segments
  (segment->painter
   (outline-segments '((0.5 0) (1 0.5) (0.5 1) (0 0.5)))))

;; Creo que he pillado el tema
