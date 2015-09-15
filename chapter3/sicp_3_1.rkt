;; #lang scheme
;; Section 3.1.1 Local State Variables

(define (prn s) (write s) (newline))

(define (assert msg pred)
  (or (and pred (prn msg) (prn 'ok))
      (error (string-join (list "error:" msg)))))

;; Exercise 3.1

(define (make-accumulator iv)
  (let ((acc iv))
    (lambda (inc) (begin (set! acc (+ acc inc)) acc))))

(assert "3.1.- Testing accumulator"
        (let ((acc (make-accumulator 5)))
          (and (eq? (acc 10) 15 )
               (eq? (acc 10) 25))))

;; Exercise 3.2

(define (make-monitored f)
  (define calls 0)
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) calls)
          ((eq? m 'reset-count) (begin (set! calls 0) 'reset-ok))
          (else (begin (set! calls (+ 1 calls)) (f m)))))
  dispatch)

(assert "3.2.- Testing monitored proc"
        (let ((s (make-monitored sqrt)))
          (and (eq? (s 100) 10)
               (eq? (s 'how-many-calls?) 1)
               (eq? (s 'reset-count) 'reset-ok)
               (eq? (s 'how-many-calls?) 0))))

;; Exercise 3.3

(define (make-account balance pass)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p pass)) (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(assert "3.3.- Testing account with password"
        (let ((acc (make-account 100 'secret-password)))
          (and (eq? ((acc 'secret-password 'withdraw) 40) 60)
               (eq? ((acc 'some-other-password 'deposit) 50)
                    "Incorrect password"))))
;; Exercise 3.4

(define (call-the-cops) "cops called")

(define (make-account-safer balance pass max-trials)
  (define acc (make-account balance pass))
  (define trials 0)
  (define (dispatch p m)
    (if (>= trials max-trials) (const (call-the-cops))
        (begin (if (not (eq? p pass))
                     (set! trials (+ 1 trials))
                     (set! trials 0))
               (acc p m))))
  dispatch)

(assert "3.4.- Testing account with password and max password tries"
        (let ((acc (make-account-safer 100 'secret-password 2)))
          (and (eq? ((acc 'secret-password 'withdraw) 40) 60)
               (eq? ((acc 'other 'withdraw) 40) "Incorrect password")
               (eq? ((acc 'secret-password 'withdraw) 40) 20)
               (eq? ((acc 'other 'withdraw) 40) "Incorrect password")
               (eq? ((acc 'other 'withdraw) 40) "Incorrect password")
               (eq? ((acc 'other 'withdraw) 40) "cops called"))))

;; 3.1.2  The Benefits of Introducing Assignment

;; from https://mitpress.mit.edu/sicp/code/ch3.scm
(define random-init 7)
;; from https://mitpress.mit.edu/sicp/code/ch3support.scm
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; Exercise 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (point-in pred x1 x2 y1 y2)
  (let* ((px (random-in-range x1 (+ x2 1)))
          (py (random-in-range y1 (+ y2 1))))
       (pred px py)))

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (* (monte-carlo trials
                  (lambda () (point-in pred x1 x2 y1 y2)))
     (* (- x2 x1) (- y2 y1))))

(define (make-circle-pred x y c)
  (lambda (px py) (<= (+ (expt (- px x) 2)
                         (expt (- py y) 2))
                      (expt c 2))))

;; with a higher resolution of points the approx is much better
(define unit-circle-pred (make-circle-pred 0 0 1000))

(define (estimate-pi2 trials)
  (* 0.000001 (estimate-integral
               unit-circle-pred -1000 1000 -1000 1000 trials)))

(prn "Estimate pi:")
(prn (estimate-pi2 10000))

;; Exercise 3.6

(define rand
  (let 
      (define r random-init)
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (lambda () (set! r (rand-update r)) r))
            ((eq? m 'reset) (lambda (x) (set! r x) x))
            (else (error "Unknown request -- RAND")))))
  dispatch)
