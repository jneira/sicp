#lang scheme

(define (prn s) (write s) (newline))

(define (assert msg pred)
  (or (and pred (prn msg) (prn 'ok))
      (error (string-join (list "error:" msg)))))

;; 3.2  The Environment Model of Evaluation

;; 3.2.1  The Rules for Evaluation

;; 3.2.2  Applying Simple Procedures

;; Exercise 3.9
(define str number->string)
(define (factorial n c)
  (prn (string-append "E" (str c) " -> n=" (str n) " (factorial)"))
  (if (= n 1)
      1
      (* n (factorial (- n 1) (+ 1 c)))))

(define (factorial2 n)
  (prn (string-append "E1 -> n=" (str n) " (factorial)"))
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (prn (string-append "E" (str (+ 1 counter)) "-> product=" (str product)
                    ", counter=" (str counter) ", max-count=" (str max-count)
                    " (fact-iter)"))
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(prn "Exercise 3.9")
(prn "Env for recursive factorial")
(factorial 6 1)
(prn "Env for iterative factorial")
(factorial2 6)

;; 3.2.3  Frames as the Repository of Local State

;; Exercise 3.10

;; Let creates an additional Env so
;; E1 -> initial amount <- E2 -> balance

;; 3.2.4  Internal Definitions

;; Exercise 3.11
(define c0 0)
(define (make-account balance)
  (set! c0 (+ 1 c0))
  (define env0 (string-append "GlobalEnv <- " "E" (str c0)))
  (define env1 "")
  (prn (string-append env0 " (make-account)"))
  (prn (string-append "   balance=" (str balance))) 
  (define c1 0)
  (define (withdraw amount)
    (set! c1 (+ 1 c1))
    (prn (string-append env1 " <- E" (str c0) "." (str c1) " (withdraw)"))
    (prn (string-append "   amount=" (str amount))) 
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! c1 (+ 1 c1))
    (prn (string-append env1 " <- E" (str c0) "." (str c1) " (deposit)"))
    (prn (string-append "   amount=" (str amount))) 
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (set! c1 (+ 1 c1))
    (set! env1 (string-append env0 " <- E" (str c0) "." (str c1)))
    (prn (string-append env1 " (dispatch)"))
    (prn (string-append "   m=" (symbol->string m))) 
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(prn "Exercise 3.11")
(define acc (make-account 50))
((acc 'deposit) 40)
((acc 'withdraw) 60)
(define acc2 (make-account 100))

;; Accounts keep local state in differente frames (E1 y E2)
;; Both only shares the global env (inlcuding the code of subprocs)