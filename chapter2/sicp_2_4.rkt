;; 2.4  Multiple Representations for Abstract Data

;; 2.4.1  Representations for Complex Numbers

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

;; 2.4.2  Tagged data

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a) 
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y) 
  (attach-tag 'polar
               (cons (sqrt (+ (square x) (square y)))
                     (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;; (define (real-part z)
;;   (cond ((rectangular? z) 
;;          (real-part-rectangular (contents z)))
;;         ((polar? z)
;;          (real-part-polar (contents z)))
;;         (else (error "Unknown type -- REAL-PART" z))))

;; (define (imag-part z)
;;   (cond ((rectangular? z)
;;          (imag-part-rectangular (contents z)))
;;         ((polar? z)
;;          (imag-part-polar (contents z)))
;;         (else (error "Unknown type -- IMAG-PART" z))))

;; (define (magnitude z)
;;   (cond ((rectangular? z)
;;          (magnitude-rectangular (contents z)))
;;         ((polar? z)
;;          (magnitude-polar (contents z)))
;;         (else (error "Unknown type -- MAGNITUDE" z))))

;; (define (angle z)
;;   (cond ((rectangular? z)
;;          (angle-rectangular (contents z)))
;;         ((polar? z)
;;          (angle-polar (contents z)))
;;         (else (error "Unknown type -- ANGLE" z))))

;; 2.4.3  Data-Directed Programming and Additivity

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (square x) (* x x))

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define table '())

(define (put op type impl)
  (define (eqcar x xs) (equal? x (car xs)))
  (let* ((op-aux (if (pair? op) (car op) op))
         (types-impls (cdr (or (assoc op table) (list op))))
         (rest-types-impls (remove type types-impls eqcar))
         (rest (remove op table eqcar))
         (new (cons (list type impl) rest-types-impls)))
    (set! table (cons (cons op new) rest)) table))

(define (get op type)
  (let* ((types-procs (assoc op table))
         (type-proc (when types-procs
                      (assoc type (cdr types-procs)))))
    (and type-proc (cadr type-proc))))

;; Exercise 2.73.

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define variable?  symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; a: Dynamic dispatch using the first elem of the expression
;; number? and variable? need exp would not be a list to be true

;; b:

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+  a1  a2))))

(define (install-sum-operator)
  (define addend car)
  (define (augend form)
    (if (> (length form) 2)
        (cons '+ (cdr form))
        (cadr form)))
  (define (deriv-sum ops var)
    (make-sum (deriv (addend ops) var)
              (deriv (augend ops) var)))
  (put 'deriv '+ deriv-sum)
  (put 'make-op '+ make-sum)
  'done)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (install-product-operator)
  (define multiplier car)
  (define (multiplicand form)
    (if (> (length form) 2)
        (cons '* (cdr form))
        (cadr form)))
  (define (deriv-product ops var)
    (make-sum
     (make-product (multiplier ops)
                   (deriv (multiplicand ops) var))
     (make-product (deriv (multiplier ops) var)
                   (multiplicand ops))))
  (put 'deriv '* deriv-product)
  'done)

(install-sum-operator)
(install-product-operator)

(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(+ x 3) 'x)
;; 1

;; c:

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list '** base exp))))

(define (install-exponent-operator)
  (define base car)
  (define exponent cadr)
  (define (deriv-exponent ops var)
    (make-product
     (make-product (exponent ops)
                   (make-exponentiation
                    (base ops)
                    (make-sum (exponent ops) -1)))
     (deriv (base ops) var)))
  (put 'deriv '** deriv-exponent)
  'done)

(install-exponent-operator)

(deriv '(* 2 (** x 2)) 'x)
;; (* 2 (* 2 x))

;;d: change the order in put an get calls

;; Exercise 2.74.

;;a
(define (get-record name file)
  (let ((record (get 'get-record
                     (type-tag file) name (contents file))))
    (when record (attach-tag (type-tag file) record))))
;; Each file has to be annotated with the id of the division
;; (attach-tag 'spain file)
 ;;b
(define (get-salary employee)
  (get 'get-salary (type-tag employee) (contents employee)))

;;c
(define (find-employee-record name files)
  (andmap (curry get-record name) files))

;;d
(define (install-new-file tag)
  (define (get-record name file) null)
  (define (get-salary employee) null)
  (put 'get-record tag get-record)
  (put 'get-salary tag get-salary))

;; Message passing


(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;; Exercise 2.75.

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (case op
      ((real-part) (* r (cos a)))
      ((imag-part) (* r (sin a)))
      ((magnitude) r)
      ((angle) a)
      (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; Exercise 2.76.
;; explicit: change impl of original procedures to add new cases
;; data-directed: add a version of each method to global table
;;                when adding a new data type, add only one generic
;;                method with adding a new operation
;; message-passing: add the possibly new operation of each object
;;                and add only one data type with all ops
