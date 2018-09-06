;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_143) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 143.

; A List-of-temperatures is one of: 
; – '()
; – (cons CTemperature List-of-temperatures)
 
; A CTemperature is a Number greater than -273.

; List-of-temperatures -> Number
; computes the average temperature

;; tests
(check-error (checked-average '()))
(check-expect
  (checked-average (cons 1 (cons 2 (cons 3 '())))) 2)

(define (checked-average alot)
  (cond
    [(empty? alot)
     (error "Error: function is applied to empty list '()")]
    [else
     (/ (sum alot) (how-many alot))]))

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