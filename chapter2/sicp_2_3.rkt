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

;; Exercise 2.59.

(define (union-set s1 s2)
  (cond
   ((null? s1) s2)
   ((null? s2) s1)
   (else 
    (union-set (adjoin-set (car s2) s1) (cdr s2)))))

;; Exercise 2.60

(define adjoin-set cons)

;; Insertion becomes easier but rest of procs which have to recurse
;; through the set will be slower (they have to go through duplicate
;; elements) but the order of growth remains the same O(n)
;; Only useful in apps doing much more insertions than other ops

;; Sets as ordered lists

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

;; Exercise 2.61

(define (adjoin-set x set)
  (cond
   ((null? set) '())
   ((= (car set) x) set)
   ((> (car set) x) (cons x set))
   (else (cons (car set) (adjoin-set x (cdr set))))))

;; Exercise 2.62

(define (union-set s1 s2)
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

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
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
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
