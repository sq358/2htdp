;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_168) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 168.

; ListOfPosns -> ListOfPosns
; For each (make-posn x y) in the input, output contains
; (make-posn x (+ y 1))

(check-expect (translate '()) '())

(check-expect
 (translate (cons (make-posn 2 4) (cons (make-posn 5 3) '())))
 (cons (make-posn 2 (+ 4 1)) (cons (make-posn 5 (+ 3 1)) '())))

(define (translate lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop)
     (cons
      (make-posn (posn-x (first lop)) (+ (posn-y (first lop)) 1))
      (translate (rest lop)))]))