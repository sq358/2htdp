;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_176) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 176.

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

(define row1-transpose (cons 11 (cons 21 '())))
(define row2-transpose (cons 12 (cons 22 '())))

(define mat1-transpose
  (cons row1-transpose (cons row2-transpose '())))

; Q. Why does transpose ask (empty? (first lln))?
; A. Because line which is a Row can be empty

; Matrix -> ListOfNumbers
; consumes a matrix and produces the first column as
; list of numbers

(check-expect (first* (cons '() '())) '())
(check-expect (first* mat1) (cons 11 (cons 21 '())))

(define (first* m)
  (cond
    [(empty? (first m)) '()]
    [else
     (cons
      (first (first m))
      (if (empty? (rest m)) '() (first* (rest m))))]))

; Matrix -> Matrix
; consumers a matrix a removes the first column resulting
; in a Matrix

(check-expect (rest* (cons '() '())) '())
(check-expect
 (rest* mat1)
 (cons
  (cons 12 '())
  (cons
   (cons 22 '()) '())))

(define (rest* m)
  (cond
    [(empty? (first m)) '()]
    [else
     (cons
      (rest (first m))
      (if (empty? (rest m)) '() (rest* (rest m))))]))