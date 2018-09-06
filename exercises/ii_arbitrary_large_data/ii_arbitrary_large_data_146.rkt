;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_146) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 146.

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
(check-expect (sum (cons 1 '())) 1)
(check-expect (sum (cons 1 (cons 1 (cons 1 '())))) 3)

(define (sum ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else (+ (first ne-l) (sum (rest ne-l)))]))
 
; List-of-temperatures -> Number 
; counts the temperatures on the given list

;; tests
(check-expect (how-many (cons 23 '())) 1)
(check-expect (how-many (cons 23 (cons 21 '()))) 2)

(define (how-many alot)
  (cond
    [(empty? (rest alot)) 1]
    [(cons? alot)
     (+ 1 (how-many (rest alot)))]))