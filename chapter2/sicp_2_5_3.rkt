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

(define p1 (make-polynomial
            'x (make-sparse-term-list (list 3 z2+3i 7))))
;; (polynomial x (3 3) (2 (complex rectangular 2 . 3)) (1 7))
(define p2 (make-polynomial
            'x (make-sparse-term-list (list 1 0 r2/3  z5+3i))))
;; (polynomial x (4 1) (2 (rational 2 . 3)) (1 (complex rectangular 5 . 3)))


(equal?
 (add p1 p2)

 '(polynomial
   x (4 1) (3 3)
   (2 (complex rectangular
               (scheme-number . 2.6666666666666665)
               scheme-number . 3))
   (1 (complex rectangular
               (scheme-number . 12) scheme-number . 3))))

(equal?
 (mul p1 p2)
 
 '(polynomial
  x (7 (scheme-number . 3))
  (6 (complex  polar (scheme-number . 3.605551275463989)
               rational 8852218891597467 . 9007199254740992))
  (5 (integer . 9))
  (4 (complex rectangular
              (rational 2298712309803691.0 . 140737488355328.0)
              rational 6192449487634433.0 . 562949953421312.0))
  (3 (complex rectangular
              (rational 6380099472108203.0 . 1125899906842624.0)
              rational 5910974510923777.0 . 281474976710656.0))
  (2 (complex polar
              (scheme-number . 40.81666326391711)
              rational 4867666120084705 . 9007199254740992))))

(define p22 (make-polynomial 'x (make-sparse-term-list '(2 2))))

;; Exercise 2.87

(put '=zero? '(polynomial) (compose null? cdr))

(define p3 (make-polynomial
            'x (make-sparse-term-list
                (list
                 (make-polynomial 'y '((2 1) (1 1)))
                 (make-polynomial 'y '((3 1) (1 1)))
                 (make-polynomial 'y '((2 1) (1 -1)))))))

(define p4 (make-polynomial
            'x (make-sparse-term-list
                (list
                 (make-polynomial 'y '((2 1) (1 -2)))
                 (make-polynomial 'y '((3 1) (1 7)))))))

(equal?
 (add p3 p4)

 '(polynomial
  x
  (3 (polynomial y (2 1) (1 1)))
  (2 (polynomial y (3 1) (2 1) (1 (scheme-number . -1))))
  (1 (polynomial y (3 1) (2 1) (1 (scheme-number . 6))))))

(equal?
 (mul p3 p4)
 
 '(polynomial
   x (5 (polynomial
         y (4 (scheme-number . 1))
                (3 (scheme-number . -1))
                (2 (scheme-number . -2))))
   (4 (polynomial
       y (5 (scheme-number . 2))
       (4 (scheme-number . -1))
       (3 (scheme-number . 8))
       (2 (scheme-number . 5))))
   (3 (polynomial
       y (6 (scheme-number . 1))
       (4 (scheme-number . 9))
       (3 (scheme-number . -3))
       (2 (scheme-number . 9))))
   (2 (polynomial
       y (5 (scheme-number . 1))
              (4 (scheme-number . -1))
              (3 (scheme-number . 7))
              (2 (scheme-number . -7))))))

;;Exercise 2.88.

(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (sub-terms L1 L2)
  (op-terms sub  L1 L2))

(put 'sub '(polynomial polynomial) 
     (lambda (p1 p2) (attach-tag 'polynomial (sub-poly p1 p2))))

(equal?
 (sub p1 p2)
 '(polynomial
   x (4 1) (3 3)
   (2 (complex rectangular
               (scheme-number . 1.3333333333333335)
                scheme-number . 3))
   (1 (complex rectangular (scheme-number . 2) scheme-number . -3))))

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
    (make-term (length term-list)
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

(define (make-dense-term-list term-list)
  (make-term-list (attach-tag 'dense term-list)))
(define (make-sparse-term-list term-list)
  (make-term-list (attach-tag 'sparse term-list)))

(define p1 (make-polynomial
            'x (make-sparse-term-list (list 3 z2+3i 7))))
(define p2 (make-polynomial
            'x (make-sparse-term-list (list 1 0 r2/3  z5+3i))))

(equal?
 (add p1 p2)
 '(polynomial
   x sparse
   (term 4 1) (term 3 3)
   (term 2 (complex rectangular
                    (scheme-number . 2.6666666666666665)
                    scheme-number . 3))
   (term 1 (complex rectangular
                    (scheme-number . 12) scheme-number . 3))))

(define default-term-list-type 'sparse)
(define (the-empty-termlist)
  (attach-tag default-term-list-type '()))

(mul p1 p2)

(define p3d (make-polynomial
             'x  (make-dense-term-list '(2 2))))
(define p1d (make-polynomial
             'x (make-dense-term-list
                 (list 3 z2+3i 7))))
(define p2d (make-polynomial
             'x (make-dense-term-list
                 (list 1 0 r2/3  z5+3i))))

(add p1d p2d)
(mul p1d p2d)
(add p1 p2d)
(add p1d p2)
(mul p1d p2)
(mul p1 p2d)

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
  (prn '********) (prn L1) (prn L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let* ((new-c (div (coeff t1) (coeff t2)))
                   (new-o (- (order t1) (order t2)))
                   (term (make-term new-o new-c))
                   (next-dividend
                    (sub-terms L1 (mul-term-by-all-terms term L2)))
                   (rest-of-result (div-terms next-dividend L2)))
              (prn term) (prn next-dividend)
              (prn '*******)
              (list (adjoin-term term
                                 (car rest-of-result))
                    (cadr rest-of-result)))))))

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

(define (mk var xs)
  (make-polynomial
   var (make-sparse-term-list xs)))

(define x^3-1 (mk 'x '(1 0 -1)))
(define x^2-1 (mk 'x '(1 -1)))

(equal?
 (div x^3-1 x^2-1)
 '((polynomial
    x
    sparse
    (term 1 (scheme-number . 1))
    (term 0 (scheme-number . -1)))
   (polynomial x sparse (term 1 (scheme-number . -2)))))

;; Exercise 2.92

;; Hierarchies of types in symbolic algebra
