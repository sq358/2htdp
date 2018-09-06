;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_153) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 153.

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

(define TEST-IMAGE
  (circle 5 "outline" "red"))

(define RED-DOT
  (circle 2 "solid" "red"))

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

(define SQUARE (rectangle 10 10 "outline" "black"))

(define LECTURE-HALL (row 8 (col 18 SQUARE)))

(define SCENE
  (place-image
   LECTURE-HALL
   (/ (image-width LECTURE-HALL) 2)
   (/ (image-height LECTURE-HALL) 2)            
   (empty-scene
    (image-width LECTURE-HALL) (image-height LECTURE-HALL))))

; ListOfPosns is one of:
; - '()
; - (cons (posn Number Number) ListOfPosns)

; ListOfPosns -> Image
; produces and image of lecture hall with red dots
; added as specified by Posns

(check-expect (add-balloons '()) SCENE)

(check-expect
 (add-balloons (cons (make-posn 10 20) '()))
 (place-image RED-DOT 10 20 SCENE))

(check-expect
 (add-balloons
  (cons (make-posn 10 20) (cons (make-posn 21 50) '())))
 (place-image RED-DOT 21 50
              (place-image RED-DOT 10 20 SCENE)))

 (define (add-balloons lop)
   (cond
     [(empty? lop) SCENE]
     [else
      (place-image
       RED-DOT
       (posn-x (first lop)) (posn-y (first lop))
       (add-balloons (rest lop)))]))
