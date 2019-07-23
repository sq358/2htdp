;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname iv_intertwined_data_328) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 328.

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
 
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
 
(define (substitute sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else
     (map (lambda (s) (substitute s old new)) sexp)]))

; Any -> Boolean
; For the given input determines if it's an atom or not

(check-expect (atom? 2) #true)
(check-expect (atom? "hello") #true)
(check-expect (atom? 'yum) #true)
(check-expect (atom? (lambda (d) #true)) #false)

(define (atom? a)
  (or (string? a)
      (number? a)
      (symbol? a)))