;; 2.5  Systems with Generic Operations

(define (prn s) (write s) (newline))

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
  (put 'make '(scheme-number) tag)
  'done)

(define (make-scheme-number n)
  ((get 'make '(scheme-number)) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (and (integer? n) (integer? d))
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))
        (cons n d)))
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
  ((get 'make '(rational)) n d))

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
(define (equ? x y) (apply-generic 'equ? x y))

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

;; Exercise 2.82

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (let ((casts (get-casts args)))
          (if (not (empty? casts))
              (apply apply-generic op (car casts))
              (error "No method for these types"
                     (list op type-tags)))))))

(define casting get-coercion)

(define (get-casts args)
  (define type-tags (map type-tag args))
  (define (cast arg to-type)
    (if (equal? (type-tag arg) to-type) arg
        (let ((casting-op
               (casting (type-tag arg) to-type))) 
          (and casting-op (casting-op arg)))))
  (define (acc-casts next-type acc)
    (let ((casted-args
           (map (lambda (arg) 
                   (cast arg next-type)) args)))
      (if (member false casted-args) acc
          (cons casted-args acc))))
  (if (and (> (length args) 1)
           (not (apply equal? type-tags)))
      (foldl acc-casts '() type-tags)
     '()))


;; Exercise 2.83

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
  (put 'equ? self (get 'equ? sn))
  (put '=zero? self (get '=zero? (car sn)))
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
  (put 'equ? self (get 'equ? sn))
  (put '=zero? self (get '=zero? (car sn)))
  'done)

(install-integer-package)
(install-real-package)

(define (make-real r) (attach-tag 'real-number r))
(define (make-integer i) (attach-tag 'integer i))

(define (install-raise-operator)
  (put 'raise '(integer)          
       (lambda (i) (make-rational i 1)))
  (put 'raise '(rational)
       (lambda (r) (make-real
               (/ (car r) (cdr r) 1.0))))
  (put 'raise '(real-number)
       (lambda (r) (make-complex-from-real-imag 
               r 0)))
  'done)

(install-raise-operator)
(define (raise x) (apply-generic 'raise x))

(define one (make-integer 1))
(define two (make-integer 2))

(raise one)
;; (rational 1 . 1)
(raise (raise one))
;; (real-number . 1.0)
(raise (raise (raise one)))
;; (complex rectangular 1.0 . 0)

;; Exercise 2.84

(define (parents type)
  (get 'parents type))

(define (derive parent child)
  (put 'parents child
       (cons parent (or (parents child) '()))))

(define (isa? from-type to-type)
  (let ((ps (parents from-type)))
    (and ps
         (or (member to-type ps)
             (ormap (lambda (f) (isa? f to-type)) ps)))))

(define (casting from-type to-type)
  (if (isa? from-type to-type)
      raise
      (get 'coercion (list from-type to-type))))

(define (install-numerical-tower)
  (derive 'rational 'integer)
  (derive 'real-number 'rational)
  (derive 'complex 'real-number)
  'done)

(install-numerical-tower)

(add one one)
;; (integer . 2)
(add one (raise one))
;; (rational 2 . 1)
(add (raise (raise one)) one)
;; (real-number . 2.0)
(add (raise (raise (raise one))) one)
;; (complex rectangular 2.0 . 0)

;; Exercise 2.85
(equ? one one)
;; #t
(equ? one (raise one))
;; #t

(define (install-project-package)
  (put 'project '(complex)          
       (lambda (c) (make-real
               (contents (real-part c)))))
  (put 'project '(real-number)
       (lambda (r) (let ((ratio (inexact->exact (contents r))))
                (make-rational
                 (numerator ratio)
                 (denominator ratio)))))
  (put 'project '(rational)
       (lambda (r) (make-integer
               (let ((r (exact->inexact
                         (/ (contents (car r))
                            (contents (cdr r))))))
                 (inexact->exact
                  (floor r))))))
  'done)

(install-project-package)

(define (project n)
  (apply-generic 'project n))

(project (raise (raise (raise one))))
;; (real-number . 1.0)
(project (raise (raise one)))
;; (rational 1 . 1)
(project (raise one))
;; (integer . 1)

(define (projectable type)
  (get 'project (list type)))

(define (lower n)
  (if (and (pair? n)
           (projectable (type-tag n)))
    (let ((res (project n)))
      (if (equ? (raise res) n)
          (lower res) n)) n))

(lower one) ;(integer . 1)
(lower (raise one)) ;(integer . 1)
(lower (make-rational 1 2)) ;(rational 1 . 2)
(lower (raise (raise one))) ;(integer . 1)
(lower (make-real 0.5)) ; (rational 1 . 2)
(lower (make-complex-from-real-imag 1 0)) ; (integer . 1)
(lower (make-complex-from-real-imag 1.5 0)) ; (rational 3 . 2)
(lower (make-complex-from-real-imag 1 1)) ; (complex rectangular 1 . 1)

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc 
        (let ((res (apply proc (map contents args))))
          (if (or (eq? op 'raise) (eq? op 'project))
              res (lower res)))
        (let ((casts (get-casts args)))
          (if (not (empty? casts))
              (apply apply-generic op (car casts))
              (error "No method for these types"
                     (list op type-tags)))))))

(define z1 (make-complex-from-real-imag 1 0))

(add one (raise one)) ; (integer . 2)
(add z1 z1)           ; (integer . 2)

;; Exercise 2.86

(define z-one (make-complex-from-real-imag one one))
;; (complex rectangular (integer . 1) integer . 1)
(define z-two (make-complex-from-real-imag two two))
;; (complex rectangular (integer . 2) integer . 2)
(define z-three (make-complex-from-mag-ang (make-real pi) one))
;; (complex polar (real-number . 3.141592653589793) integer . 1)

(define (install-complex-package-v2)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (let ((res (make-from-real-imag (add (real-part z1) (real-part z2))
                                     (add (imag-part z1) (imag-part z2)))))
      res))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
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
  (put 'equ? '(complex complex)
       (lambda (i j) (and (equ? (real-part i) (real-part j))
                     (equ? (imag-part i) (imag-part j)))))
  (put '=zero? '(complex)
       (lambda (x) (=zero? (magnitude x))))
  (put 'project '(complex)          
       (lambda (c) (make-real
               (contents (let ((rp (real-part c)))
                           (if (eq? (type-tag rp) 'rational)
                               (raise rp) rp))))))
  'done)

(install-complex-package-v2)

(define (scheme-number-coercions)
  (put-coercion 'scheme-number 'integer
                (lambda (n) (make-integer (contents n))))
  (put-coercion 'scheme-number 'rational
                (lambda (n)
                  (if (real? n)
                      (let ((r (inexact->exact n)))
                        (make-rational (numerator r)
                                       (denominator r)))
                      (make-rational (contents n) 1))))
  (put-coercion 'scheme-number 'real-number
                (lambda (n) (make-real (contents n))))
  (put-coercion 'scheme-number 'complex
                (lambda (n) (make-complex-from-real-imag
                        (contents n) 0))))

(scheme-number-coercions)
(add z-one z-one)
;; (complex rectangular (integer . 2) integer . 2)
(add z1 z1)
;; (integer . 2)
(sub z-one z-one)
;; (integer . 0)
(sub z-one z-two)
;; (complex rectangular (integer . -1) integer . -1)
;(add z-two z-three)
;; error cos: expects argument of type <number>; given (integer . 1)

(define (install-trigonometric-package)
  (define tag (curry attach-tag 'real-number))
  (define tag-cos (compose tag cos))
  (put 'cos '(scheme-number) tag-cos)
  (put 'cos '(integer) tag-cos)
  (put 'cos '(rational)
       (lambda (r) (tag (cos (/ (car r) (cdr r))))))
  (put 'cos '(real-number) tag-cos)
  (define tag-sin (compose tag sin))
  (put 'sin '(scheme-number) tag-sin)
  (put 'sin '(integer) tag-sin)
  (put 'sin '(rational)
       (lambda (r) (tag (sin (/ (car r) (cdr r))))))
  (put 'sin '(real-number) tag-sin)
  (define tag-atan (compose tag atan))
  (put 'atan '(scheme-number) tag-atan)
  (put 'atan '(scheme-number scheme-number) tag-atan)
  (put 'atan '(integer) tag-atan)
  (put 'atan '(integer integer) tag-atan)
  (put 'atan '(rational)
       (lambda (r) (tag (atan (/ (car r) (cdr r))))))
  (put 'atan '(rational rational)
       (lambda (r s) (tag (atan (/ (car r) (cdr r))
                           (/ (car s) (cdr s))))))
  (put 'atan '(real-number) tag-sin)
  (put 'atan '(real-number real-number) tag-sin)
   'done)

(install-trigonometric-package)

(define cosine (curry apply-generic 'cos))
(define sine (curry apply-generic 'sin))
(define arctan (curry apply-generic 'atan))

(define (square x)
  (mul x x))

(define (install-sqrt-package)
  (define default (compose sqrt contents))
  (put 'sqrt '(scheme-number) default)
  (put 'sqrt '(integer) default)
  (put 'sqrt '(rational)
       (lambda (r) (/ (default (car r))
                 (default (cdr r))))))

(install-sqrt-package)
(define sqrt (curry apply-generic 'sqrt))

(define (install-rectangular-package-v2)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (add (square (real-part z))
               (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (tag x) (attach-tag 'rectangular x))
  (define (make-from-mag-ang r a) 
    (cons (* r (cosine a)) (* r (sine a))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  'done)

(install-rectangular-package-v2)

(define (install-polar-package-v2)
  ;; internal procedures
  (define (real-part z)
    (mul (car z) (cosine (cdr z))))
  (define (imag-part z)
    (mul (car z) (sine (cdr z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (sum (square x) (square y)))
          (arctan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(install-polar-package-v2)

(add z-two z-three)
;;(complex rectangular
;;         (rational 16651653194101815 . 4503599627370496)
;;         rational 10456365435335067 . 2251799813685248)

(sub z-three z-two)
;;(complex rectangular
;;         (rational -1362745315380169 . 4503599627370496)
;;         rational 1449166180594075 . 2251799813685248)
(mul z-three z-two)
;;(complex polar
;;         (rational 5632023223300212547393616123015 .
;;                   633825300114114700748351602688)
;;         rational
;;         2010179625846179 . 1125899906842624)
(div z-three z-two)
;;(complex polar
;;         (rational 884279719003555/2251799813685248 .
;;                   281474976710656/6369051672525773)
;;         rational
;;         241620187839069 . 1125899906842624)
