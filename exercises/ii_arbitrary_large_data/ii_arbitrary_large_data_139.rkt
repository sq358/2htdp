;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_139) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 139.

; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)

; List-of-amounts -> Integer
; computes the sum of amounts in list-of-amounts
;; tests
(check-expect (checked-sum '()) 0)

(check-expect
 (checked-sum
  (cons 12 (cons 13 (cons 10 '())))) 35)

(check-error
 (checked-sum
  (cons 1 (cons 2 (cons -3 '())))))

(define (checked-sum loa)
  (cond
    [(empty? loa) 0]
    [(pos? loa) (+ (first loa) (checked-sum (rest loa)))]
    [else
     (error "List of amounts can't contain negative numbers")]))


; List-of-names -> Boolean
; determines whether all numbers in list of names are
; positive

;; tests
(check-expect (pos? '()) #true)
(check-expect (pos? (cons 1 '())) #true)
(check-expect
 (pos? (cons 1 (cons -1 '())))
 #false)
(check-expect
 (pos? (cons 1 (cons 2 (cons 2 '()))))
 #true)

(define (pos? lon)
  (cond
    [(empty? lon) #true]
    [else
     (and (> (first lon) 0) (pos? (rest lon)))]))

(pos? (cons 5 '()))