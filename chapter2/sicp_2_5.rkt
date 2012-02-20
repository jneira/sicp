;; 2.5  Systems with Generic Operations

;; 2.5.1  Generic Arithmetic Operations

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make '(scheme-number)
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (/ (numer x) (denom y))
              (/ (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make '(rational)
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Exercise 2.77.
(set! table '())
(install-polar-package)
(install-rectangular-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define z (make-complex-from-real-imag 3 4))
;; (magnitude z)
;; raise expected error

(define (extend-complex-package-with-selectors)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(extend-complex-package-with-selectors)

(magnitude z)

;; trace
(apply-generic 'magnitude '(complex rectangular 3 . 4))
((get 'magnitude '(complex)) '(rectangular 3 . 4))
(magnitude '(rectangular 3 . 4))
(apply-generic 'magnitude '(rectangular 3 . 4))
((get 'magnitude '(rectangular)) '(3 . 4))
((lambda (x) (sqrt (+ (square (real-part z))
             (square (imag-part z))))) '(3 . 4))

;; Exercise 2.78

;; (add (make-scheme-number 1) (make-scheme-number 2))
;; 3
;; (add 1 2)
;; Bad tagged datum -- TYPE-TAG 1

(define (type-tag datum)
  (cond
   ((pair? datum) (car datum))
   ((number? datum) 'scheme-number)
   (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond
   ((pair? datum) (cdr datum))
   ((number? datum) datum)
   (else (error "Bad tagged datum -- CONTENTS" datum))))

;; (add 1 2)
;; 3

;; Exercise 2.79.

(define (equ? x y) (apply-generic 'equ? x y))

(define (install-number-equality-package)
  (put 'equ? '(complex complex)
       (lambda (i j) (and (= (real-part i) (real-part j))
                     (= (imag-part i) (imag-part j)))))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (put 'equ? '(rational rational)
       (lambda (x y)
         (if (= 0 (numer y))
             (= 0 (numer x))
             (= (/ (numer x) (numer y))
                (/ (denom x) (denom y))))))
  (put 'equ? '(scheme-number scheme-number) =)
  'done)

(install-number-equality-package)

;; Exercise 2.80

(define (=zero? x) (apply-generic '=zero? x))

(define (install-zero-predicate-package)
  (put '=zero? '(complex)
       (lambda (x) (= 0 (magnitude x))))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (put '=zero? '(rational)
       (lambda (x) (= 0 (numer x))))
  (put '=zero? '(scheme-number) (curry = 0))
  'done)

(install-zero-predicate-package)

(define (put-coercion from-type to-type casting)
  (put 'coercion (list from-type to-type) casting))

(define (get-coercion from-type to-type)
  (get 'coercion (list from-type to-type)))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; Exercise 2.81.

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;; a)

(define (^ x y) (apply-generic '^ x y))
;;following added to Scheme-number package
(put '^ '(scheme-number scheme-number)
     (lambda (x y) (expt x y)))            ; using primitive expt

;; The apply-generic funtion diverges cause the base case stopping
;; recursion doesnt exist anymore. It tries to coerce complex-number
;; of both arguments forever

(^ 2 3)
;;(^ (make-complex-from-real-imag 2 2)
;;   (make-complex-from-real-imag 2 2))
 
;; b)

(put-coercion 'complex 'complex #f)
(put-coercion 'scheme-number 'scheme-number #f)

;; if the type of both args are the same the coercion is no necesary

;; c)

(define (apply-generic op args)
  (let ((type-tags (map type-tag args)))
   (if (and (= (length args) 2)
            (not (apply equal? type-tags)))
       (let* ((type1 (car type-tags))
              (type2 (cadr type-tags))
              (t1->t2 (get-coercion type1 type2))
              (t2->t1 (get-coercion type2 type1))
              (a1 (car args))
              (a2 (cadr args)))
         (cond (t1->t2
                (apply-generic op (t1->t2 a1) a2))
               (t2->t1
                (apply-generic op a1 (t2->t1 a2)))
               (else
                (error "No method for these types"
                       (list op type-tags)))))
       (error "No method for these types"
              (list op type-tags)))))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (let ((coercions (get-coercions args)))
          (if (not (empty? coercions))
              (apply apply-generic op (car coercions))
              (error "No method for these types"
                     (list op type-tags)))))))

;; Exercise 2.82

(define (get-coercions args)
  (define type-tags (map type-tag args))
  (define (coerce arg to-type)
    (if (equal? (type-tag arg) to-type) arg
        (let ((coercion-op
               (get-coercion (type-tag arg) to-type))) 
          (and coercion-op (coercion-op arg)))))
  (define (acc-coercions next-type acc)
    (let ((coerced-args
           (map (lambda (arg) 
                   (coerce arg next-type)) args)))
      (if (member false coerced-args) acc
          (cons coerced-args acc))))
  (if (and (> (length args) 1)
           (not (apply equal? type-tags)))
      (foldl acc-coercions '() type-tags)
     '()))

;; Exercise 2.83

(define (install-raise-operator)
  (put 'raise '(integer)          
       (lambda (i)
         (make-rational i 1)))
  (put 'raise '(rational)
       (lambda (r) (attach-tag
               'real-number
               (/ (car r) (cdr r) 1.0))))
  (put 'raise '(real-number)
       (lambda (r) (make-complex-from-real-imag 
               r 0)))
  'done)

(install-raise-operator)
(define (raise x) (apply-generic 'raise x))

;; Exercise 2.84

(define (get-coercion from-type to-type)
  (if (is-a? from-type to-type)
      raise
      (get 'coercion (list from-type to-type))))

(define (is-a? from-type to-type)
  (let ((ps (parents from-type)))
    (and ps
         (or (member to-type ps)
             (ormap (lambda (f) (is-a? f to-type)) ps)))))

(define (parents type)
  (get 'parents type))

(define (derive parent child)
  (put 'parents child
       (cons parent (or (parents child) '()))))

(define (install-numerical-tower)
  (derive 'rational 'integer)
  (derive 'real-number 'rational)
  (derive 'complex 'real-number)
  'done)

(define (install-integer-package)
  (define self '(integer integer))
  (define (tag x) (attach-tag 'integer x) )
  (define sn '(scheme-number scheme-number))
  (define (trans op)
    (compose tag contents (get op sn)))
  (put 'add self (trans 'add))
  (put 'sub self (trans 'sub))
  (put 'mul self (trans 'mul))
  (put 'div self (trans 'div))
  'done)


(define (install-real-package)
  (define self '(real-number real-number))
  (define (tag x) (attach-tag 'real-number x) )
  (define sn '(scheme-number scheme-number))
  (define (trans op)
    (compose tag contents (get op sn)))
  (put 'add self (trans 'add))
  (put 'sub self (trans 'sub))
  (put 'mul self (trans 'mul))
  (put 'div self (trans 'div))
  'done)

(install-numerical-tower)
(install-integer-package)
(install-real-package)

(define one (attach-tag 'integer 1))
(define two (attach-tag 'integer 2))

(raise one)
;; (rational 1 . 1)
(raise (raise one))
;; (real-number . 1.0)
(raise (raise (raise one)))
;; (complex rectangular 1.0 . 0)

(add one one)
;; (integer . 2)
(add one (raise one))
;; (rational 2 . 1)
(add (raise (raise one)) one)
;; (real-number . 2.0)
(add (raise (raise (raise one))) one)
;; (complex rectangular 2.0 . 0)

;; Exercise 2.85

