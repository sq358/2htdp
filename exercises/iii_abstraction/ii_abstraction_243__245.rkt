;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ii_abstraction_243__245) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 243.

(define (f x) x)

(cons f '())
(f f)
(cons f (cons 10 (cons (f 10) '())))

; Explain why they are (not) values?
; Because values are static and fixed, but
; functions are functions

; Exercise 244.

; because g can be a function accepting a function
; as argument
(define (g x) (x 10))

; because u can be a function accepting a function
(define (u x) (x u))

; because t is a function
(define (t x y) (x 'a y 'b))

; Exercise 245.

; Function Function -> Boolean
; determines whether the two input functions produce
; the same results for 1.2, 3 and -5.775

(define (func1 n)
  (cond
    [(or (= n 1.2) (= n 3) (= n -5.775)) n]
    [else (* n n)]))

(define (func2 n)
  (cond
    [(or (= n 1.2) (= n 3) (= n -5.775)) n]
    [else (+ n n)]))

(define (func3 n) (* (+ n 5) 10))

(check-expect (function=at-1.2-3-and-5.775? func1 func2) #true)
(check-expect (function=at-1.2-3-and-5.775? func1 func3) #false)

(define (function=at-1.2-3-and-5.775? f1 f2)
  (and
      (= (f1 1.2) (f2 1.2))
      (= (f1 3) (f2 3))
      (= (f1 -5.775) (f2 -5.775))))

; defining function=? is not mathemitacally possible because
; we simply cannot test the result of two functions for all
; possible inputs, even if we assume they both accept only
; one input (argument)
  

