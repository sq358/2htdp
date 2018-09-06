;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_138) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 138.

; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)

; List-of-amounts -> Integer
; computes the sum of amounts in list-of-amounts
;; tests
(check-expect (sum '()) 0)

(check-expect
 (sum
  (cons 12 (cons 13 (cons 10 '())))) 35)

(check-error
 (sum
  (cons 1 (cons 2 (cons -3 '())))))

(define (sum loa)
  (cond
    [(empty? loa) 0]
    [(< (first loa) 0)
     (error "List of amounts can't contain negative numbers")]
    [else (+ (first loa) (sum (rest loa)))]))