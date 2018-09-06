;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_147) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 147.

; An NEList-of-Booleans is one of: 
; – (cons Boolean '())
; – (cons Boolean NEList-of-Booleans)
; interpretation non-empty lists of booleans 
 
; NEList-of-booleans -> Boolean
; determines whether all booleans in list of booleans
; are #true

(check-expect (all-true (cons #false '())) #false)
(check-expect
 (all-true (cons #true (cons #true '()))) #true)
(check-expect
 (all-true (cons #false (cons #true (cons #true '())))) false)

(define (all-true lob)
  (cond
    [(empty? (rest lob)) (first lob)]
    [else
     (and (first lob) (all-true (rest lob)))]))

; NEList-of-Booleans -> Booleans
; determines wheter any of the booleans in list of booleans
; are #true

(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false '())) #false)
(check-expect
 (one-true (cons #false (cons #true '()))) #true)
(check-expect
 (one-true (cons #false (cons #false '()))) #false)
(check-expect
 (one-true (cons #false (cons #false (cons #true '())))) #true)

(define (one-true lob)
  (cond
    [(empty? (rest lob)) (first lob)]
    [else
     (or (first lob) (one-true (rest lob)))]))