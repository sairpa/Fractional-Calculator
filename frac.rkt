#lang racket

; GROUP - 7

;  ----------------------------------------
; | Team Members:                         |
; | Elavenil P - CB.EN.U4CSE18115         | 
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


;Welcome Screen

(display "Welcome to Fractional Calculator")
(display "\n")
(display "Operations Supported:")
(display "\n")
(display "1. Addition")
(display "\n")
(display "2. Subtraction")
(display "\n")
(display "3. Multiplication")
(display "\n")
(display "4. Division")
(display "\n")
(display "5. Exponentiation")
(display "\n")
(display "6. Inverse")
(display "\n")
(display "7. Equality")
(display "\n")
(display "8. Conversion of Types")
(display "\n")
(display "Input your choice as a number")
(display "\n")




;INPUT TYPE

;NORMAL FRACTION

(define (normal_fraction_input)
  (display "Input the fraction as 2 numbers - the numerator and the denominator")
  (display "\n")
  (print "Enter the numerator")
  (define a (read))
  (display "\n")
  (print "Enter the denominator")
  (define b (read))
  (display "\n")
  (define norm (cons a b))
  norm
)
  

;MIXED FRACTION

(define (mixed_fraction_input)
  (display "Input the fraction as 3 numbers - the whole number part, the numerator and the denominator")
  (display "\n")
  (print "Enter the first part of the mixed fraction")
  (display "\n")
  (define x (read))
  (print "Enter the numerator of the mixed fraction")
  (display "\n")
  (define y (read))
  (print "Enter the denominator of the mixed fraction")
  (display "\n")
  (define z (read))
  (define mix (cons x (cons y z)))
  (define fracs (mixed-to-normal1 mix))
  fracs  
  )


;COMPLEX FRACTION 
  
(define (complex_fraction_input)
  (display "\n")
  (display "Type of Complex Fraction")
  (display "\n")
  (display "1. Both numerator and denominator fractional")
  (display "\n")
  (display "2. Numerator fractional")
  (display "\n")
  (display "3. Denominator fractional")
  (display "\n")
  (define com (read))
  (if (equal? com 1)(complex_fraction_input_3)
  (if (equal? com 2)(complex_fraction_input_1)
      (complex_fraction_input_2)                                                
      )
      )
  )
 
 
(define (complex_fraction_input_1)
  (display "Input the numerator as fraction as 2 numbers")
  (define num1 (read))
  (define denom1 (read))
  (display "Input the denominator as integer")
  (define denom_int (read))
  (define complex (cons (cons num1 denom1) denom_int))
  (define sim (/ (caar complex)(* (cdar complex ) (cdr complex))))
  (define comp (simplify (numerator sim) (denominator sim)))
  comp
  )

(define (complex_fraction_input_2)
  (display "Input the numerator as integer")
  (define num_int (read))
  (display "Input the fraction in denominator as 2 numbers")
  (define num2 (read))
  (define denom2 (read))
  (define complex (cons num_int(cons num2 denom2)))
  (define sim (/ (* (car complex ) (cddr complex))(cadr complex)))
  (define comp (simplify (numerator sim) (denominator sim)))
  comp
  )

(define (complex_fraction_input_3)
  (display "Input the fraction in numerator as 2 numbers")
  (define num1 (read))
  (define denom1 (read))
  (display "Input the  fraction in denominator as 2 numbers")
  (define num2 (read))
  (define denom2 (read))
  (define complex (cons (cons num1 denom1) (cons num2 denom2)))
  (define sim (/ (* (caar complex ) (cddr complex))(* (cadr complex ) (cdar complex))))
  (define comp (simplify (numerator sim) (denominator sim)))
  comp
  )

(define (simplify-mixed mix) (if (pair? mix) (print (car mix))  (print "no")))

;DISPLAY PROCEDURE

;normal-----------------------------
(define (normal-display norm)
(display (car norm))
(display "\n")
(display "-")
(display "\n")
(display (cdr norm))
(display "\n")
  )

;Mixed Use print ()
(define (mixed-display mix)
(display "\t")
(display (cddr mix))
(display "\n")
(display (car mix))
(display "\t-")
(display "\n")
(display "\t")
(display (cadr mix))
  )

;complex-----------------------------
(define (complex-display norm)
(display (car norm))
(display "\n")
(display "-")
(display "\n")
(display (cdr norm))
(display "\n")
  )

; Input Switcher

(define (input-switcher)
(display "Choose the type of fraction")
(display "\n")
(display "1. Proper/Improper Fraction")
(display "\n")
(display "2. Mixed Fraction")
(display "\n")
(display "3. Complex Fraction")
(display "\n")
 (define ftype (read))
  (cond
  [(equal? ftype 1) (
                       normal_fraction_input
                       )]
  [(equal? ftype 2) (
                       mixed_fraction_input
                       )]
  [(equal? ftype 3) (
                       complex_fraction_input
                       )]
  ))


;HELPER FUNCTIONS

(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (modulo a b))
      )
  )


(define (GCDf a b)
  (if (< a b)
      (GCDf b a)
      (display "")
      )
  (if (< (abs b) 0.001)
      a
      (GCDf b (- a (*(exact-floor (/ a b)) b)))
      ))


(define (simplify n d)
  (if (and (natural? n)(natural? d))
      (begin
        (if (> n d)
      (let ()
      (define z (GCD n d)) (define disp (cons (/ n z) (/ d z)))
  disp)
      (let ()
      (define z (GCD d n)) (define disp (cons (/ n z) (/ d z)))
  disp)
      ))
      (begin
        (if (> n d)
      (let ()
      (define z (GCDf n d)) (define disp (cons (exact-round (/ n z)) (exact-round (/ d z))))
  disp)
      (let ()
      (define z (GCDf d n)) (define disp (cons (exact-round (/ n z)) (exact-round (/ d z))))
  disp)
      ))
      
  ))
 
;OPERATIONS

;ADDITION

(define (addition)
  (define x (input-switcher))
  (define y (input-switcher))
  (define n1 (+ (* (car x) (cdr y))(* (car y) (cdr x))))
  (define d1 (* (cdr x)(cdr y)))
  (define res (simplify n1 d1))
  (display "\n")
  (display "Result is")
  (display "\n")
  (normal-display res)
  )
  
;SUBRACTION

(define (subraction) 
  (define x (input-switcher))
  (define y (input-switcher))
  (define n1 (- (* (car x) (cdr y))(* (car y) (cdr x))))
  (define d1 (* (cdr x)(cdr y)))
  (define res (simplify n1 d1))
  (display "\n")
  (display "Result is")
  (display "\n")
  (normal-display res)
  )

;MULTIPLICATION

(define (multiplication)
  (define x (input-switcher))
  (define y (input-switcher))
  (define n1 (* (car x) (car y)))
  (define d1 (* (cdr x)(cdr y)))
  (define res (simplify n1 d1))
  (display "\n")
  (display "Result is")
  (display "\n")
  (normal-display res)
  )

;DIVISION

(define (division)
  (define x (input-switcher))
  (define y (input-switcher))
  (define n1 (* (car x) (cdr y)))
  (define d1 (* (cdr x)(car y)))
  (define res (simplify n1 d1))
  (display "\n")
  (display "Result is")
  (display "\n")
  (normal-display res)
  )

;INVERSION

(define (inversion)
  (define x (input-switcher))
  (display "\n")
  (display "Result is")
  (display "\n")
  (normal-display (cons (cdr x)(car x)))
  )

;EQUALITY

(define (equality)
  (define x (input-switcher))
  (define y (input-switcher))
  (define e1 (/ (car x)(cdr x)))
  (define e2 (/ (car y)(cdr y)))
  (display "\n")
  (if (= e1 e2)
      "Fractions are equal"
      "Fractions are not equal")
  (display "\n")
  )

  
;EXPONENT
      
(define (exponent)
  (define x (input-switcher))
  (display "\n")
  (display "Enter the power")
  (define exp (read))
  (define n1 (expt (car x) exp))
  (define d1 (expt (cdr x) exp))
  (define res (simplify n1 d1))
  (display "\n")
  (display "Result is")
  (display "\n")
  (normal-display res)
  )


; CONVERSION OF TYPES

; Normal to Mixed Fraction :D

(define (convert-to-mixed)
  (define x (normal_fraction_input))
  (define n (car x))
   (define d (cdr x))
   (define newn (quotient n d))
   (define re (remainder n d))
   (define gcd (GCD n d))
   (define newd (/ d gcd))
  (display "\n")
  (display "Result is")
  (display "\n")
  (mixed-display (cons newn (cons newd re )))
   )

; Mixed to Normal Fraction :D

(define (mixed-to-normal)
  (define x (mixed_fraction_input))
  (display "\n")
  (display "Result is")
  (display "\n")
  (normal-display x)
  )

(define (mixed-to-normal1 x)
  (define num (cadr x))
  (define denom (cddr x))
  (define re (car x))
  (define newn (+ (* re denom) num))
  (define frac (cons newn denom))
  frac
  )

(define (conversion)
  (display "1. Improper Fraction to Mixed Fraction")
  (display "\n")
  (display "2. Mixed Fraction to Improper Fraction")
  (display "\n")
  (define conv (read))
  (cond
    [(equal? conv 1)(convert-to-mixed)]
    [(equal? conv 2)(mixed-to-normal)]
    )
  )

; Operation Chooser

(define operation (read-line))

(cond
  [(equal? operation "1") (addition)]
  [(equal? operation "2") (subraction)]
  [(equal? operation "3") (multiplication)]
  [(equal? operation "4") (division)]
  [(equal? operation "5") (exponent)]
  [(equal? operation "6") (inversion)]
  [(equal? operation "7") (equality)]
  [(equal? operation "8") (conversion)]
  )

