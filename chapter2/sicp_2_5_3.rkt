(load "sicp_2_5.rkt")
;; 2.5.3  Example: Symbolic Algebra 

(define (make-poly variable term-list)
  (cons variable term-list))
(define (variable p) (car p))
(define (term-list p) (cdr p))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff)
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (op-terms op L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term 
                   t1 (op-terms op (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term 
                   t2 (op-terms op L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (op (coeff t1) (coeff t2)))
                   (op-terms op (rest-terms L1)
                             (rest-terms L2)))))))))

(define (add-terms L1 L2)
  (op-terms add L1 L2))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (let ((fst (mul-term-by-all-terms (first-term L1) L2) )
            (rst (mul-terms (rest-terms L1) L2)))
        (add-terms fst rst))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let* ((t2 (first-term L))
             (term (make-term (+ (order t1) (order t2))
                              (mul (coeff t1) (coeff t2))))
             (restTerms (mul-term-by-all-terms t1 (rest-terms L))))
        (adjoin-term term restTerms))))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))


(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-polynomial-package)

(define (term-list p)
  (if (eq? (type-tag p) 'polynomial)
      (term-list (contents p))
      (cdr p)))

(define (variable p)
  (if (eq? (type-tag p) 'polynomial)
      (variable (contents p))
      (car p)))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define z2+3i (make-complex-from-real-imag 2 3))
(define z5+3i (make-complex-from-real-imag 5 3))
(define r2/3 (make-rational 2 3))

(define (map-indexed f xs)
  (if (null? xs) '()
      (let ((nxt (map-indexed f (cdr xs))))
        (if (=zero? (car xs)) nxt
            (cons (f (- (length xs) 1) (car xs))
                  nxt)))))

(define make-sparse-term-list
  (curry map-indexed (lambda (i x) (make-term i x))))

(define (mk var xs)
  (make-polynomial
   var (make-sparse-term-list xs)))

(define (mkd var xs)
  (make-polynomial
   var (make-dense-term-list xs)))

(define p1 (mk 'x (list 3 z2+3i 7)))
;; (polynomial x (3 3) (2 (complex rectangular 2 . 3)) (1 7))
(define p2 (mk 'x (list 1 0 r2/3  z5+3i)))
;; (polynomial x (4 1) (2 (rational 2 . 3)) (1 (complex rectangular 5 . 3)))


(assert "testing add polynomials"
 (equal? (add p1 p2)
   '(polynomial x (3 1) (2 3)
     (1 (complex rectangular
                 (scheme-number . 2.6666666666666665) scheme-number . 3))
     (0 (complex rectangular
                 (scheme-number . 12) scheme-number . 3)))))

(assert "testing mult polynomials"
 (equal? (mul p1 p2)
  '(polynomial x
    (5 (scheme-number . 3))
    (4 (complex polar (scheme-number . 3.605551275463989)
      rational 8852218891597467 . 9007199254740992))
    (3 (integer . 9))
    (2 (complex rectangular
          (rational 2298712309803691.0 . 140737488355328.0)
          rational 6192449487634433.0 .  562949953421312.0))
    (1 (complex rectangular
         (rational 6380099472108203.0 . 1125899906842624.0)
          rational 5910974510923777.0 .  281474976710656.0))
    (0 (complex polar (scheme-number . 40.81666326391711)
          rational 4867666120084705 . 9007199254740992)))))

(define p22 (mk 'x '(2 2)))

;; Exercise 2.87

(put '=zero? '(polynomial) (compose null? cdr))

(define p3
  (mk 'x (list (mk 'y '(1 1))
               (mk 'y '(1 0 1))
               (mk 'y '(1 -1)))))

(define p4
  (mk 'x (list (mk 'y '(1 -2))
               (mk 'y '(1 0 7)))))

(assert "testing add two polys with polynomil coeffs"
 (equal?
  (add p3 p4)
  '(polynomial x
    (2 (polynomial y (1 1) (0 1)))
    (1 (polynomial y (2 1) (1 1) (0 (scheme-number . -1))))
    (0 (polynomial y (2 1) (1 1) (0 (scheme-number . 6)))))))

(assert "testing mult two polys with polynomials coeffs"
 (equal?
  (mul p3 p4)
  '(polynomial x
    (3 (polynomial y (2 (scheme-number . 1))
                   (1 (scheme-number . -1))
                   (0 (scheme-number . -2))))
    (2 (polynomial y (3 (scheme-number . 2))
                   (2 (scheme-number . -1))
                   (1 (scheme-number . 8))
                   (0 (scheme-number . 5))))
    (1 (polynomial y (4 (scheme-number . 1))
                   (2 (scheme-number . 9))
                   (1 (scheme-number . -3))
                   (0 (scheme-number . 9))))
    (0 (polynomial y (3 (scheme-number . 1))
                   (2 (scheme-number . -1))
                   (1 (scheme-number . 7))
                   (0 (scheme-number . -7)))))))

;;Exercise 2.88.

(define (neg-terms tl)
  (if (empty-termlist? tl) tl
      (let* ((ft (first-term tl))
             (v (neg (coeff ft))))
        (adjoin-term (make-term (order ft) v)
                     (neg-terms (rest-terms tl))))))

(define (neg-poly p)
  (make-poly (variable p)
             (neg-terms (term-list p))))

(define (install-negation-package)
  (put 'neg '(scheme-number) (compose make-scheme-number -))
  (put 'neg '(integer) (compose make-integer -))
  (put 'neg '(real-number) (compose make-real -))
  (put 'neg '(rational)
       (lambda (r) (make-rational (neg (car r)) (neg (cdr r)))))
  (put 'neg '(complex)
       (lambda (i) (make-complex-from-real-imag
               (neg (real-part i)) (neg (imag-part i)))))
  (define (tag p) (attach-tag 'polynomial p))
  (put 'neg '(polynomial) (compose tag  neg-poly))
  'done)

(install-negation-package)

(define neg (curry apply-generic 'neg))

(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (neg-terms (term-list p2))))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))


(put 'sub '(polynomial polynomial) 
     (lambda (p1 p2) (attach-tag 'polynomial (sub-poly p1 p2))))

(assert "testing substract polys"
 (equal?
  (sub p3 p4)
  '(polynomial x
    (2 (polynomial y (1 1) (0 1)))
    (1 (polynomial y (2 1) (1 (scheme-number . -1))
                           (0 (scheme-number . 3))))
    (0 (polynomial y (2 (scheme-number . -1)) (1 1)
                     (0 (scheme-number . -8)))))))

(assert "testing substract zero polys"
  (equal? (sub (make-polynomial 'x (make-sparse-term-list '(0 0)))
               (make-polynomial 'x (make-sparse-term-list '(0 0))))
          '(polynomial x)))
;; Exercise 2.89. and 2.90

(define (install-term-package)
  (put 'order '(term) car)
  (put 'coeff '(term) cadr)
  'done)

(install-term-package)

(define order (curry apply-generic 'order))
(define coeff (curry apply-generic 'coeff))
(define (make-term order coeff)
  (attach-tag 'term (list order coeff)))

(define (install-sparse-term-list-package)
  (define tag (curry attach-tag 'sparse))
  (define remove-zero-coeff
    (curry filter (compose not =zero? coeff)))
  (define make-sparse-term-list
    (compose remove-zero-coeff
             (curry map-indexed
                    (curry make-term))))
  (define  (make-term-list xs)
    (tag (make-sparse-term-list xs)))
  (define (adjoin-term term term-list)
    (let ((t (attach-tag 'term term)))
      (tag (if (=zero? (coeff t))
               term-list
               (cons t term-list)))))
  (put 'make-term-list '(sparse) make-term-list)
  (put 'adjoin-term '(term sparse) adjoin-term)
  (put 'first-term '(sparse) car)
  (put 'rest-terms '(sparse) (compose tag cdr))
  (put 'empty-termlist? '(sparse) null?)
  'done)

(install-sparse-term-list-package)

(define (install-dense-term-list-package)
  (define tag (curry attach-tag 'dense))
  (define (make-term-list xs) (tag xs))
  (define (adjoin-term term lst) 
    (let ((t (attach-tag 'term term)))
      (tag (cons (coeff t) lst))))
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))
  (put 'make-term-list '(dense) make-term-list)
  (put 'adjoin-term '(term dense) adjoin-term)
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense) (compose tag cdr))
  (put 'empty-termlist? '(dense) null?)
  'done)

(install-dense-term-list-package)

(define make-term-list  
  (curry apply-generic 'make-term-list))
(define adjoin-term
  (curry apply-generic 'adjoin-term))
(define first-term
  (curry apply-generic 'first-term))
(define rest-terms
  (curry apply-generic 'rest-terms))
(define empty-termlist?
  (curry apply-generic 'empty-termlist?))

(define (make-term-list-type tag terms)
  (make-term-list (attach-tag tag terms)))
(define (make-dense-term-list terms)
  (make-term-list-type 'dense terms))
(define (make-sparse-term-list terms)
  (make-term-list-type 'sparse terms))

(define p1 (mk 'x (list 3 z2+3i 7)))
(define p2 (mk 'x (list 1 0 r2/3  z5+3i)))

(assert "testing add polynomials after dense/sparse term list"
 (equal?
   (add p1 p2)
   '(polynomial x sparse
      (term 3 1)
      (term 2 3)
      (term 1 (complex rectangular
                       (scheme-number . 2.6666666666666665)
                       scheme-number . 3))
      (term 0 (complex rectangular
                       (scheme-number . 12) scheme-number . 3)))))

(define default-term-list-type 'sparse)
(define (empty-termlist type)
  (make-term-list-type type '()))
(define (the-empty-termlist)
  (empty-termlist default-term-list-type))

(assert "testing mult polys after dense/sparse term list"
  (equal? (mul p1 p2)
  '(polynomial x sparse
     (term 5 (scheme-number . 3))
     (term 4 (complex polar
       (scheme-number . 3.605551275463989)
       rational 8852218891597467 . 9007199254740992))
     (term 3 (integer . 9))
     (term 2 (complex rectangular
       (rational 2298712309803691.0 . 140737488355328.0)
       rational 6192449487634433.0 . 562949953421312.0))
     (term 1 (complex rectangular
       (rational 6380099472108203.0 . 1125899906842624.0)
       rational 5910974510923777.0 . 281474976710656.0))
     (term 0 (complex polar
        (scheme-number . 40.81666326391711)
        rational 4867666120084705 . 9007199254740992)))))

(define p3d (make-polynomial
             'x  (make-dense-term-list '(2 2))))
(define p1d (make-polynomial
             'x (make-dense-term-list
                 (list 3 z2+3i 7))))
(define p2d (make-polynomial
             'x (make-dense-term-list
                 (list 1 0 r2/3  z5+3i))))

(assert "testing add y mult between diff term list representations"
        (begin (add p1d p2d)
               (mul p1d p2d)
               (add p1 p2d)
               (add p1d p2)
               (mul p1d p2)
               (mul p1 p2d)
               true))

;; Exercise 2.91

;; Division can be performed via long division. That is, divide the
;; highest-order term of the dividend by the highest-order term of the
;; divisor. The result is the first term of the quotient. Next,
;; multiply the result by the divisor, subtract that from the
;; dividend, and produce the rest of the answer by recursively
;; dividing the difference by the divisor. Stop when the order of the
;; divisor exceeds the order of the dividend and declare the dividend
;; to be the remainder. Also, if the dividend ever becomes zero,
;; return zero as both quotient and remainder.


(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let* ((new-c (div (coeff t1) (coeff t2)))
                   (new-o (- (order t1) (order t2)))
                   (term (make-term new-o new-c)))
              (let* ((mult (mul-term-by-all-terms term L2))
                     (next-dividend (add-terms L1 (neg-terms mult)))
                     (rest-of-result (div-terms next-dividend L2)))
                (list (adjoin-term term (car rest-of-result))
                      (cadr rest-of-result))))))))

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (map (curry  make-poly (variable p1))
           (div-terms (term-list p1)
                      (term-list p2)))
      (error "Polys not in same var -- DIV-POLY"
             (list p1 p2))))

(define (install-polynomial-ext-package)
  (define (tag p) (attach-tag 'polynomial p))
  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
  'done)

(install-polynomial-ext-package)

(define x^3-1 (mk 'x '(1 0 0 -1)))
(define x^2-1 (mk 'x '(1 0 -1)))

(assert "testing div polynomials"
 (equal?
  (div x^3-1 x^2-1)
  '((polynomial x sparse (term 1 (scheme-number . 1)))
    (polynomial x sparse (term 1 (scheme-number . 1)) (term 0 -1)))))

;; Hierarchies of types in symbolic algebra

;; Exercise 2.92.  By imposing an ordering on variables, extend the
;; polynomial package so that addition and multiplication of
;; polynomials works for polynomials in different variables. (This is
;; not easy!) 

(define (separate-terms term-list)
  (if (empty-termlist? term-list) '()
      (let ((ft (first-term term-list)))
        (append (if (=zero? (coeff ft)) '()
                    (expand-term ft)) 
                (separate-terms (rest-terms term-list))))))

(define (expand p)
   (let* ((tl (term-list p))  (ts (separate-terms tl))
         (tl-type (type-tag tl)))
    (map (lambda (t) (term->poly (variable p) tl-type t))
         ts))) 

(define (expand-term t)
   (if (eq? (type-tag (coeff t)) 'polynomial)
      (let* ((ec (expand (coeff t)))
             (f (lambda (p) (make-term (order t) p))))
        (map f ec))
      (list t)))


(define (term->poly var term-type term)
  (make-polynomial var
   (adjoin-term term (empty-termlist term-type))))

(define (any->poly var term-type ord coeff)
  (term->poly var term-type (make-term ord coeff)))


(define (polynomial? p)
  (eq? (type-tag p) 'polynomial))

(define (order-by-var var p)
  (define (next-term poly)
    (first-term (term-list poly)))
  (define (get-order poly)
    (if (polynomial? poly)
        (let ((nxt-term (next-term poly)))
         (if (eq? (variable poly) var)
             (order nxt-term)
             (get-order (coeff nxt-term))))
        0))
  (define (remove-var poly)
    (if (polynomial? poly)
        (let* ((tlist (type-tag (term-list poly)))
               (varp (variable poly))
               (nxt-term (next-term poly))
               (nxt (coeff nxt-term))
               (nxt-ord (order nxt-term)))
          (if (eq? varp var) nxt
              (any->poly varp tlist nxt-ord (remove-var nxt))))
        poly))
  (let* ((order (get-order p))
         (wout-var (remove-var p))
         (tlist (type-tag (term-list p))))
    (any->poly var tlist order wout-var)))

(define (flip f)
  (lambda (x y) (f y x)))

(define (install-number->poly-package)
  (define (number->poly-op op n p)
    (let ((tlist (type-tag (term-list p)))
          (v (variable p)))
      (op (any->poly v tlist 0 n)
          (attach-tag 'polynomial p))))
  (put 'add '(scheme-number  polynomial)
       (curry number->poly-op add))
  (put 'add '(polynomial scheme-number)
       (flip (curry number->poly-op add)))
  (put 'mul '(scheme-number  polynomial)
       (curry number->poly-op mul))
  (put 'mul '(polynomial scheme-number)
       (flip (curry number->poly-op mul)))
  (put 'div '(scheme-number  polynomial)
       (curry number->poly-op div))
  (put 'div '(polynomial scheme-number)
       (flip (curry number->poly-op (flip div))))
  (put 'equ? '(scheme-number  polynomial)
       (curry number->poly-op equ?))
  (put 'equ? '(polynomial scheme-number)
       (flip (curry number->poly-op equ?)))
  'done)

(install-number->poly-package)

(define (combine polys)
  (foldr add (car polys) (cdr polys)))

(define (canonical p1 p2)
  (list p1 (combine
            (map (curry order-by-var (variable p1))
                 (expand p2)))))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (apply add-poly (canonical p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (apply mul-poly (canonical p1 p2))))

;; xy+yx =x(2y)
(define p_1x (mk 'x '(0 1)))
(define p_1y (mk 'y '(0 1)))
(define p_x (mk 'x '(1 0)))
(define p_y (mk 'y '(1 0)))
(define p_xy (mul p_x p_y))
(define p_yx (mul p_y p_x))

(define p_x2y (add p_xy p_yx))
(define p_y2x (add p_yx p_xy))

(define p_x^2+1 (add (mul p_x p_x) p_1x))
(define p_y^2+1 (add (mul p_y p_y) p_1y))

(assert "testing mult polynomial with diff vars"
 (equal?
  (mul p_x^2+1 p_y^2+1)
  '(polynomial x sparse
               (term 2 (polynomial y sparse
                                   (term 2 (scheme-number . 1))
                                   (term 0 (scheme-number . 1))))
               (term 0 (polynomial y sparse
                                   (term 2 (scheme-number . 1))
                                   (term 0 (scheme-number . 1)))))))

;; (y+1)x^2+(y^2+1)x+(y-1)
(define p3
  (mk 'x (list (mk 'y '(1 1))
               (mk 'y '(1 0 1))
               (mk 'y '(1 -1)))))
;; (x-2)y+(x^2+7)
(define p4
  (mk 'y (list (mk 'x '(1 -2))
               (mk 'x '(1 0 7)))))

(define p5
  (mk 'x (list
    (mk 'y (list (mk 'z '(3 0 0 -3)) 0 2)) 0)))

(define p6
  (mk 'z (list
     (mk 'x (list (mk 'y '(2 0 2)) 0 0)))))

(assert "testing add polys with diff vars"
 (equal?
  (add p5 p6)
  '(polynomial x sparse
      (term 2 (polynomial z sparse
                 (term 0 (polynomial y sparse
                            (term 2 2)
                            (term 0 2)))))
      (term 1 (polynomial y sparse
                 (term 2 (polynomial z sparse
                            (term 3 3) (term 0 -3)))
                 (term 0 2))))))

;; Extended exercise: Rational functions

;; Exercise 2.93

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (install-rational-generic-package)
  ;; internal procedures
  (define (add-rat x y)
    (let* ((nx (numer x)) (dy (denom y))
           (ny (numer y)) (dx (denom x))
           (n (add (mul nx dy) (mul ny dx)))
           (d (mul dx dy)))
      (make-rat n d)))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (div (numer x) (denom y))
              (div (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y)  (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make '(rational)
       (lambda (n d)  (tag (make-rat n d))))
  'done)

(install-rational-generic-package)

(define p7 (mk 'x '(1 0 1)))
(define p8 (mk 'x '(1 0 0 1)))
(define rf (make-rational p7 p8))

(define (install-polynomial-equ-package)
  (define empty? empty-termlist?)
  (define (eq-term-list tl1 tl2)
    (or (and (empty? tl1) (empty? tl2))
        (and (not (or (empty? tl1) (empty? tl2)))
             (let* ((t1 (first-term tl1))
                    (t2 (first-term tl2)))
               (and (equ? (coeff t1) (coeff t2))
                    (eq-term-list (rest-terms tl1) (rest-terms tl2)))))))
   (put 'equ? '(polynomial polynomial) 
       (lambda (p1 p2) (eq-term-list (cdr p1) (cdr p2))))
  'done)

(install-polynomial-equ-package)

(assert "testing add rational functions"
 (equal?
  (add rf rf)
  '(rational (polynomial x sparse
               (term 5 (scheme-number . 2))
               (term 3 (scheme-number . 2))
               (term 2 (scheme-number . 2))
               (term 0 (scheme-number . 2)))
             polynomial x sparse
               (term 6 (scheme-number . 1))
               (term 3 (scheme-number . 2))
               (term 0 (scheme-number . 1)))))

;; Exercise 2.94

(define (remainder-terms a b)
  (car (cdr (div-terms a b))))

(define (gcd-terms a b)
  (if (empty-termlist? b) a
      (gcd-terms b (remainder-terms a b))))

(define (gcd-poly a b)
  (if (same-variable? (variable a) (variable b))
      (make-poly (variable a)
                 (gcd-terms (term-list a)
                            (term-list b)))
      (error "Polys not in same var -- GCD-POLY"
             (list a b))))

(define (install-gcd-package)
  (define (tag p) (attach-tag 'polynomial p))
  (put 'greatest-common-divisor
       '(integer integer) gcd)
  (put 'greatest-common-divisor
       '(scheme-number scheme-number) gcd)
  (put 'greatest-common-divisor
       '(polynomial polynomial)
       (lambda (p q) (tag (gcd-poly (tag p) (tag q)))))
  'done)

(install-gcd-package)

(define greatest-common-divisor  
  (curry apply-generic 'greatest-common-divisor))

(define p2-94-1 (mk 'x '(1 -1 -2 2 0)))
(define p2-94-2 (mk 'x '(1 0 -1 0)))
(greatest-common-divisor p2-94-1 p2-94-2)

;; Exercise 2.95

(define p2-95-1 (mk 'x '(1 -2 1)))
(define p2-95-2 (mk 'x '(11 0 7)))
(define p2-95-3 (mk 'x '(13 5)))

(define q2-95-1 (mul p2-95-1 p2-95-2))
(define q2-95-2 (mul p2-95-1 p2-95-3))

(define gcd2-95
  (greatest-common-divisor q2-95-1 q2-95-2))

;; Exercise 2.96 a

(define (pseudoremainder-terms p q)
  (let* ((c (coeff (first-term q)))
         (o1 (order (first-term p)))
         (o2 (order (first-term q)))
         (ifactor (^ c (+ 1 o1 (- o2))))
         (pf (term-list (mul ifactor (make-polynomial 'x p)))))
    (remainder-terms pf q)))

(define (gcd-terms a b)
  (if (empty-termlist? b) a
      (gcd-terms b (pseudoremainder-terms a b))))

(define gcd2-96
  (greatest-common-divisor q2-95-1 q2-95-2))

(assert "testin gcd polys with common factors"
 (equal? gcd2-96
    '(polynomial x sparse
           (term 2 (scheme-number . 1458))
           (term 1 (scheme-number . -2916))
           (term 0 (scheme-number . 1458)))))

;; Exercise 2.96 b

(define (coeffs tl)
  (if (empty-termlist? tl) '()
      (cons (coeff (first-term tl))
            (coeffs (rest-terms tl)))))

(define (remove-common-factors tl)
  (let* ((r (foldl greatest-common-divisor 0 (coeffs tl))))
    (term-list (car (div (make-polynomial 'x tl) r)))))

(define (gcd-terms a b)
  (if (empty-termlist? b) a
      (remove-common-factors
       (gcd-terms b (pseudoremainder-terms a b)))))

(define gcd2-96b
  (greatest-common-divisor q2-95-1 q2-95-2))

(assert "testing gcd polys removing common factors"
  (equal? gcd2-96b
     '(polynomial x sparse
            (term 2 (scheme-number . 1))
            (term 1 (scheme-number . -2))
            (term 0 (scheme-number . 1)))))

;; exercise 2.97
;; a
(define (reduce-terms n d)
  (let* ((g (gcd-terms n d))
         (mk (curry make-polynomial 'x)))
    (cons (term-list (car (div (mk n) (mk g))))
          (term-list (car (div (mk d) (mk g)))))))

(define (reduce-poly a b)
  (if (same-variable? (variable a) (variable b))
      (let ((mk (curry make-polynomial (variable a)))
            (r (reduce-terms (term-list a)
                             (term-list b))))
        (cons (mk (car r)) (mk (cdr r))))
      (error "Polys not in same var -- REDUCE-POLY"
             (list a b))))
;; b
(define (install-reduce-package)
  (define (tag p) (attach-tag 'polynomial p))
  (put 'reduce '(integer integer)
       reduce-integers)
  (put 'reduce '(scheme-number scheme-number)
       reduce-integers)
  (put 'reduce '(polynomial polynomial)
       (lambda (p q) (reduce-poly (tag p) (tag q))))
  'done)

(install-reduce-package)

(define reduce  
  (curry apply-generic 'reduce))

(define make-rat reduce)

(define p1-97b (mk 'x '(1 1)))
(define p2-97b (mk 'x '(1 0 0 -1)))
(define p3-97b (mk 'x '(1 0)))
(define p4-97b (mk 'x '(1 0 -1)))

(define rf1-97b (make-rational p1-97b p2-97b))
(define rf2-97b (make-rational p3-97b p4-97b))

(assert "testing add two rational functions"
  (equal? (add rf1-97b rf2-97b)
    '(rational
      (polynomial x sparse
        (term 3 (scheme-number . -1))
        (term 2 (scheme-number . -2))
        (term 1 (scheme-number . -3))
        (term 0 (scheme-number . -1)))
      polynomial x sparse
        (term 4 (scheme-number . -1))
        (term 3 (scheme-number . -1))
        (term 1 (scheme-number . 1))
        (term 0 (scheme-number . 1)))))

