;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_166) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 166.

(define-struct employee [name number])
; Employee is a structure:
;  (make-employee String Number)
; interpretation (make-employee s n) is an employee
; with name s and number s

(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work Employee Number Number)
; interpretation (make-work n r h) combines the name 
; with the pay rate r and the number of hours h

(define-struct paycheck [employee amount])
; Paycheck is a structure:
;   (make-paycheck Employee Number)
; interpretation:
;   (make-paycheck (make-employee s n) n
; is a paycheck with:
; - employee information that contains employee name
;   s and number s
; - amount n

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees

; Low -> List-of-paychecks
; computes the weekly paychecks for the given records

(check-expect (wage*.v4 '()) '())

(check-expect
  (wage*.v4
    (cons
     (make-work (make-employee "Robby" 123) 11.95 39) '()))
  (cons
   (make-paycheck (make-employee "Robby" 123) (* 11.95 39)) '()))
 
(define (wage*.v4 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low) (cons (wage.v4 (first an-low))
                          (wage*.v4 (rest an-low)))]))
 
; Work -> Paycheck
; computes the paycheck for the given work record w
(define (wage.v4 w)
  (make-paycheck (work-employee w)
  (* (work-rate w) (work-hours w))))