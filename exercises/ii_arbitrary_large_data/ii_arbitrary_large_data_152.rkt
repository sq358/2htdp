;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_152) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 152.

(define TEST-IMAGE
  (circle 10 "solid" "red"))

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; N Image -> Image
; for natural number n and image im, produces a
; vertical arrangement of n copies of image im

(check-error (col 0 TEST-IMAGE))

(check-expect
 (col 1 TEST-IMAGE) TEST-IMAGE)

(check-expect
 (col 2 TEST-IMAGE)
 (above TEST-IMAGE TEST-IMAGE))

(check-expect
 (col 3 TEST-IMAGE)
 (above
  TEST-IMAGE
  (above TEST-IMAGE TEST-IMAGE)))

(define (col n im)
  (cond
    [(= n 0)
     (error "number of copies must be bigger than 0")]
    [(= n 1) im]
    [(> n 1)
     (above im (col (sub1 n) im))]))

; N Image -> Image
; for natural number n and image im, produces a
; horizental arrangement of n copies of image im

(check-error (row 0 TEST-IMAGE))

(check-expect
 (row 1 TEST-IMAGE) TEST-IMAGE)

(check-expect
 (row 2 TEST-IMAGE)
 (beside TEST-IMAGE TEST-IMAGE))

(check-expect
 (row 3 TEST-IMAGE)
 (beside
  TEST-IMAGE
  (beside TEST-IMAGE TEST-IMAGE)))

(define (row n im)
  (cond
    [(= n 0)
     (error "number of copies must be bigger than 0")]
    [(= n 1) im]
    [(> n 1)
     (beside im (row (sub1 n) im))]))

