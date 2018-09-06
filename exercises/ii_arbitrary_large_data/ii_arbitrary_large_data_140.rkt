;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_140) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 140.

; A List-of-booleans is one of: 
; – '()
; – (cons Boolean List-of-booleans)

; List-of-booleans -> Boolean
; determines whether all booleans in list of booleans
; are #true

(check-expect (all-true '()) #true)
(check-expect (all-true (cons #false '())) #false)
(check-expect
 (all-true (cons #true (cons #true '()))) #true)
(check-expect
 (all-true (cons #false (cons #true (cons #true '())))) false)

(define (all-true lob)
  (cond
    [(empty? lob) #true]
    [else
     (and (first lob) (all-true (rest lob)))]))