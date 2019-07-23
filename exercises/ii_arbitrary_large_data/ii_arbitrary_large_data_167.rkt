;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_167) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 167.

; ListOfPosns -> Number
; computes the sum of list-of-posns posn x-cordinates

(check-expect (sum '()) 0)

(check-expect
 (sum (cons (make-posn 2 4) (cons (make-posn 5 3) '())))
 7)

(define (sum lop)
  (cond
    [(empty? lop) 0]
    [(cons? lop)
     (+ (posn-x (first lop)) (sum (rest lop)))]))