;; 1.3  Formulating Abstractions with Higher-Order Procedures

;; 1.3.2 Constructing Procedures Using Lambda

;; Exercise 1.34

;; El rastreo seria:
;; (f f) -> (f 2) -> (2 2)
;; Este ultimo provocaria un error

(define (f g) (g 2))

;; Using let to create local variables

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))


;; 1.3.3  Procedures as General Methods

;; Finding roots of equations by the half-interval method

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y) (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

;; Finding fixed points of functions

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; Exercise 1.35

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

;; Exercise 1.36.

(define (approx)
  (fixed-point (lambda (x)
                 (/ (log 1000) (log x)))
               2.0))

(define (approx-avg)
  (fixed-point (lambda (x)
                 (average x (/ (log 1000) (log x))))
               2.0))

;; Exercise 1.37.

(define (cont-frac n d k)
  (define (rec i)
    (if (> i k) 1
        (/ (n i)
           (+ (d i) (rec (+ i 1))))))
  (rec 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

;; Exercise 1.38

(define (e-cf k)
  (cont-frac
   (lambda (i) 1.0)
   (lambda (i)
     (case (modulo i 3)
       ((1 0) 1)
       ((2) (* 2 (+ 1 (quotient i 3))))))
   10))

;; Exercise 1.39

(define (tan-cf x k)
  (cont-frac
   (lambda (i) (if (= i 1) x (- (* x x))))
   (lambda (i) (- (* 2 i) 1))
   k))

;; 1.3.4  Procedures as Returned Values

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

;; Newton's method
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

;; Abstractions and first-class procedures

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

;; Exercise 1.40.

(newtons-method (cubic 1 1 1) 1)

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;; Exercise 1.41.

(define (double f) (lambda (x) (f (f x))))
(define (inc x) (+ 1 x))
(((double (double double)) inc) 5)

;; 21 5 +  2^2^2 inc = 16

;; Exercise 1.42

(define (compose f g) (lambda (x) (f (g x))))
((compose (lambda (x) (* x x)) inc) 6)

;; Exercise 1.43
(define (repeated f k)
  (if (= k 0) (lambda (x) x)
      (lambda (x) (f ((repeated f (- k 1)) x)))))

(define (repeated f k)
  (if (= k 0) (lambda (x) x)
      (compose f (repeated f (- k 1)))))

((repeated (lambda (x) (* x x)) 2) 5)

;; Exercise 1.44

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (repeated-smooth f k)
  ((repeated smooth k) f))

;; Exercise 1.45.

(define (nth-root x n)
  (fixed-point-of-transform
   (lambda (y) (/ x (expt y (- n 1))))
   (repeated average-damp (- n 2))
   1.0))g

;; Exercise 1.46.

(define (iterative-improve good-enough? improve)
  (lambda (guess) 
    (if (good-enough? guess) guess
        ((iterative-improve
          good-enough?
          improve) (improve guess)))))

(define (sqrt x  guess)
  ((iterative-improve
    (lambda (g) (< (abs (- (* g g) x)) 0.001))
    (lambda (g) (average g (/ x g))))
   guess))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (x) (< (abs (- x (f x)))
              tolerance))
    (lambda (x) (f x)))
   first-guess))
