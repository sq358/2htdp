;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_145) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 145.

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures 
 
; A CTemperature is a Number greater than -273.

; NEList-of-temperatures -> Boolean
; produces #true if temperatures are in descending order
; otherwise, returns #false

;; tests
(check-expect (sorted>? (cons 4 '())) #true)

(check-expect
 (sorted>? (cons 4 (cons 3 (cons 2 '()))))
 #true)

(check-expect
 (sorted>? (cons 4 (cons 5 (cons 2 '()))))
 #false)

(define (sorted>? nel)
  (cond
    [(empty? (rest nel)) #true]
    [else
     (and
      (> (first nel) (first (rest nel)))
      (sorted>? (rest nel)))]))