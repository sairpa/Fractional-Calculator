#lang racket

; GROUP - 7

;  ----------------------------------------
; | Team Members:                         |
; | Elavenil P - CB.EN.U4CSE18114         | 
; | Hari Hara Sudhan  - CB.EN.U4CSE18119  |
; | R Pranav Ajay - CB.EN.U4CSE18140      |
; | Prathish S - CB.EN.U4CSE18141         |
;  ----------------------------------------

; Problem - 8
; A Program to simulate Fractional Calaculator using data and procedural abstraction

; MODULE 1 It is expected that the calculator should be capable of handling proper fraction, improper fraction and mixed fractions. 
; MODULE 2 The calculator should be capable of doing all the necessary arithmetic: addition, subtraction, multiplication, division, inversion, equality, conversion between types of fractions etc.
; MODULE 3 The calculator should reduce the resultant fractions to their lowest terms
; MODULE 4 As said above, the calculator should seemingly handle all the three types of fractions: proper, improper and mixed fractions. It is also expected to handle complex fractions as well.;
; MODULE 5 Note that complex fractions are those fractions where the numerator, denominator, or both contain a fraction.
; MODULE 6 It is expected to handle the power of fractions (Note that the powers (exponents) could be either integral or fractional) as well
; MODULE 7 The calculator should display the results in a clutter free mathematical standard format.


;;--------------------------------------------------------------------------------------------------------
;;                                              CODE                                                     |
;;--------------------------------------------------------------------------------------------------------


(define (numerator x)
  (car x )
)

(define (denominator x)
  (cdr x)
)




(print "Enter the first part of the mixed fraction")
(define x (read-line))
(define y (read-line))
(define z (read-line))

(define mix (cons x (cons y z)))

(define (simplify-mixed) (if (pair? mix) (print (car mix))  (print "no")))


