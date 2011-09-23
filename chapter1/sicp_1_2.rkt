;; 1.1  The Elements of Programming

;; 1.2  Procedures and the Processes They Generate

;; 1.2.1  Linear Recursion and Iteration

;; linear recursive
(define (recursive-factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; linear iterative
(define (iterative-factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;;Exercise 1.9
(define (dec x) (- x 1))
(define (inc x) (+ x 1))

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a 1) b))))

;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; recursive

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
;; iterative

;;Exercise 1.10.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 256))
;; (A 0 1024)
;; 2048

(A 2 4)

;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2)))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 8))
;; (A 1 16)
;; 65536

(A 3 3)

;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 (A 1 (A 2 1)))
;; (A 2 (A 1 2))
;; (A 2 (A 0 (A 1 1)))
;; (A 2 (A 0 2))
;; (A 2 4)
;; 65536

(define (A0 n) (A 0 n))
;; 2n
(define (A1 n) (A 1 n))
;; 2^(n^1)
(define (A2 n) (A 2 n))
;; 2^(n^2)

;; 1.2.2 Tree recursion

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;;Example: Counting change

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;;Exercise 1.11.  A function f is defined by the rule that f(n) = n if
;;n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a
;;procedure that computes f by means of a recursive process. Write a
;;procedure that computes f by means of an iterative process.

(define (f-1-11 n)
  (if (< n 3) n
      (+ (f-1-11 (- n 1))
         (f-1-11 (- n 2))
         (f-1-11 (- n 3)))))

;; 3
;; (+ 2 1 0) = 3
;; 4
;; (+ 3 2 1) = 6
;; 5
;; (+ 6 3 2)  = 11
;; 6
;; (+ 11 6 3) = 20
;; 7
;; (+ 20 11 5) = 36

(define (f-1-11-2 n)
  (f-1-11-2-iter 2 1 0 n))

(define (f-1-11-2-iter n n1 n2 count)
  (if (= count 0) n2
      (f-1-11-2-iter (+ n n1 n2)
                     n
                     n1
                     (- count 1))))

;;Exercise 1.12.  The following pattern of numbers is called Pascal's triangle.
;;     1
;;    1 1
;;   1 2 1
;;  1 3 3 1
;; 1 4 6 4 1
;;The numbers at the edge of the triangle are all 1, and each number
;;inside the triangle is the sum of the two numbers above it.35 Write
;;a procedure that computes elements of Pascal's triangle by means of
;;a recursive process.

(define (pascal-triangle-element row col)
  (when (and (>= row 0) (>= col 0) (>= row col))
    (if (or (= col 0) (= col row)) 1
        (+ (pascal-triangle-element (- row 1) (- col 1))
           (pascal-triangle-element (- row 1) col)))))

;;It is easy to express Euclid's Algorithm as a procedure:

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; from twitter reply
(define (member-of-2? a lst1 lst2)
  (if (or (null? lst1) (null? lst2)) #f
    (or
     (and (eq? a (car lst1))
          (or (eq? a (car lst2))
              (member-of-2? a (list a) (cdr lst2))))
     (member-of-2? a (cdr lst1) lst2))))

;; Exercise 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;; Segun la formula de calculo de orden de crecimiento:
;; T(n)=
;; cn^k si 1<=n<b
;; aT(n/b)+cn^k si n>b
;; T(n)= O(n^k) ,si a<b^k
;;       O(n^k*log n), si a=b^k
;;       O(n^logba), si a>b^k
;; 5 llamadas 
;; En este caso a=1, b=3, k=0 O(log n)

;; 1.2.4 Exponentation

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product))))

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (let ((r (fast-expt b (/ n 2)))) (* r r)))
        (else (* b (fast-expt b (- n 1))))))

;; a=1, b=2, k=0 O(log n)

;;Exercise 1.16

(define (f-expt-i b n a) 
  (cond ((= n 0) a)
        ((even? n) (f-expt-i (* b b) (/ n 2) a))
        (else (f-expt-i b (- n 1) (* b a)))))       

;;Exercise 1.17

(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

;; Exercise 1.18

(define (fast-mult-iter a b r)
  (cond ((= b 0) r)
        ((even? b)
         (fast-mult-iter (double a) (halve b) r))
        (else (fast-mult-iter a (- b 1) (+ a r)))))

(= (fast-mult 7 5) (fast-mult-iter 7 5 0))

;; Exercise 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   <??>      ; compute p'
                   <??>      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; http://www.billthelizard.com/2010/01/sicp-exercise-119-computing-fibonacci.html

;; 1.2.5  Greatest Common Divisors
;; It is easy to express Euclid's Algorithm as a procedure:

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Exercise 1.20
;; http://www.billthelizard.com/2010/01/sicp-exercise-120-gcd.html

;; 1.2.6  Example: Testing for Primality

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next-divisor divisor)
  (+ divisor 1))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; The fermat test

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Exercise 1.21.  Use the smallest-divisor procedure to find the
;; smallest divisor of each of the following numbers: 199, 1999,
;; 19999.

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; Exercise 1.22

(define (timed-prime?  n) (time (prime? n)))

(define (search-for-primes n m)
  (filter prime? (build-list (- m n)
                             (lambda (x) (+ n x)))))

;; Exercise 1.23

(define (next-divisor d) (if (= d 2) 3 (+ d 2)))

;; Exercise 1.24

(define (fast-search-for-primes n m)
  (filter (lambda (n) (fast-prime? n 1)) (build-list (- m n)
                                    (lambda (x) (+ n x)))))

;; Exercise 1.25

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; Exercise 1.26

;; El codigo se evalua 2 veces mientras que en square se evalua una
;; vez

;; Exercise 1.27

;; Numbers that fool the Fermat test are called Carmichael numbers,
;; and little is known about them other than that they are extremely
;; rare. There are 255 Carmichael numbers below 100,000,000. The
;; smallest few are 561, 1105, 1729, 2465, 2821, and 6601.

(define (carmichael? n)
  (andmap (lambda (x) (= x (expmod x n n)))
          (build-list (- n 1) (lambda (x) (+ x 1)))))

(andmap carmichael? '(561 1105 1729 2465 2821 6601))

;; Exercise 1.28

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((and (not (= base 1)) (not (= base (- m 1)))
              (= (square base) (remainder 1 m))) 0)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; 1.3.1  Procedures as Arguments

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(define (cube x) (* x x x))

(integral cube 0 1 0.01)
;; .24998750000000042
(integral cube 0 1 0.001)
;; .249999875000001

;; Exercise 1.29

(define (integral-simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define (factor k) (if (even? k) 2 4))
  (define (y k) (* (factor k) (f (+ a (* k h)))))
   (* (/ h 3)
     (+  (y 0)
         (sum y 1 inc (dec n))
         (y n))))

(integral-simpson-rule cube 0 1 100)
;; 19/75 0.25333
(integral-simpson-rule cube 0 1 1000)
;; 751/3000 0.25003

;; Exercise 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ result (term a)))))
  (iter a 0))

;; Exercise 1.31
;; a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (id x) x)

(define (fact x)
  (product id 1 inc x))

(define (pi n)
  (define (square x) (* x x))
  (define (factor x i) (+ (* 2 x) i))
  (define (term x i) (square (factor x i)))
  (* (/ 8 (factor n 2))
     (product
      (lambda (x) (/ (term x 2) (term x 1)))
      1 inc n)))
;; b
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* result (term a)))))
  (iter a 1))

;; Exercise 1.32

(define (accumulate combiner neutral term a next b)
  (if (> a b)
      neutral
      (combiner (term a)
         (accumulate combiner neutral term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate combiner neutral term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
                    (combiner (term a) result))))
  (iter a neutral))

;; Exercise 1.33

(define (filtered-accumulate
         combiner pred neutral term a next b)
  (if (> a b)
      neutral
      (combiner
       (if (pred a) (term a) neutral)
       (filtered-accumulate
        combiner pred neutral term (next a) next b))))

(define (sum-primes-square a b)
  (filtered-accumulate + prime? 0 square a inc b))

(define (product-relative-primes n)
  (define (relative-prime? i) (= 1 (gcd i n)))
  (define (id x) x) (define (inc x) (+ 1 x))
  (filtered-accumulate * relative-prime? 1 id 1 inc n))
