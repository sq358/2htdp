;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_192) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 192.

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)

; Q. Exercise 192. Argue why it is acceptable to use last on Polygons.
; Also argue why you may adapt the template for connect-dots to last:
;
; A. Because designing last for Polygons does not require us to solve
; a more general problem. In addition we can use the template for
; connect-dots because connect-dots uses NeLoP as input and based on
; the argument that every input for the Polygons is also an input to
; NeLoP, solving a problem for NeLoP also solves the problem for
; Polygons; basically, we're solving a more general problem.

; Polygon -> Posn
; extracts the last item from p

(check-expect
 (last (list (make-posn 3 4) (make-posn 4 5) (make-posn 9 5)))
 (make-posn 9 5))

(check-expect
 (last
  (list (make-posn 3 4) (make-posn 4 5) (make-posn 9 5) (make-posn 3 4)))
 (make-posn 3 4))

(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))