;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_144) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 144.

;;  Q. Will sum and how-many work for NEList-of-temperatures
;;  even though they are designed for inputs from
;;  List-of-temperatures?
;;
;;  A. Both functions work as expected because their
;;  computation involves (rest alon) that can be '()

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures 
 
; A CTemperature is a Number greater than -273.

; List-of-temperatures -> Number
; computes the average temperature

; NEList-of-temperatures -> Number
; computes the average temperature 

;; tests
(check-expect (average (cons 1 (cons 2 (cons 3 '()))))
              2)
 
(define (average ne-l)
  (/ (sum ne-l)
     (how-many ne-l)))

;; When average applies to '() empty list, the result of
;; function execution is '/: division by zero', hence handling
;; the empty condition
 
; List-of-temperatures -> Number 
; adds up the temperatures on the given list

;; tests
(check-expect (sum '()) 0)
(check-expect (sum (cons 1 '())) 1)
(check-expect (sum (cons 1 (cons 1 (cons 1 '())))) 3)

(define (sum alot)
  (cond
    [(empty? alot) 0]
    [else (+ (first alot) (sum (rest alot)))]))
 
; List-of-temperatures -> Number 
; counts the temperatures on the given list

;; tests
(check-expect (how-many '()) 0)
(check-expect (how-many (cons 23 '())) 1)
(check-expect (how-many (cons 23 (cons 21 '()))) 2)

(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [(cons? alot)
     (+ 1 (how-many (rest alot)))]))