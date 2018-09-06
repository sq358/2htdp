;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_159) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 159.

(require 2htdp/universe)

(define-struct pair [balloon# lob])
; A Pair is a structure (make-pair N List-of-posns)
; A List-of-posns is one of:
; - '()
; - (cons Posn List-of-posns)
; interpretation (make-pair n lob) means n balloons
; must yet to be thrown and added to lob

; NumberOfBalloons is
; 0
; (add1 NumberOfBalloons)
; interpretation is the number of balloons the students want to throw

(define HEIGHT 220) ; distances in terms of pixels 
(define WIDTH 30)
(define XSHOTS (- (/ WIDTH 2) 5))

(define COLS 10)
(define ROWS 20)

(define TEST-IMAGE
  (circle 5 "outline" "red"))

(define RED-DOT
  (circle 2 "solid" "red"))

; N Image -> Image
; for natural number n and image im, produces a
; vertical arrangement of n copies of image im

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
    [(= n 0) im]
    [(= n 1) im]
    [(> n 1)
     (above im (col (sub1 n) im))]))

; N Image -> Image
; for natural number n and image im, produces a
; horizental arrangement of n copies of image im

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
    [(= n 0) im]
    [(= n 1) im]
    [(> n 1)
     (beside im (row (sub1 n) im))]))

; Lecture Hall
(define SQUARE (rectangle 10 10 "outline" "black"))

(define LECTURE-HALL (row ROWS (col COLS SQUARE)))

(define SCENE
  (place-image
   LECTURE-HALL
   (/ (image-width LECTURE-HALL) 2)
   (/ (image-height LECTURE-HALL) 2)            
   (empty-scene
    (image-width LECTURE-HALL) (image-height LECTURE-HALL))))

; N -> Pair
(define (riot n)
  (big-bang (make-pair n '())
    [on-tick toc]
    [to-draw throw-balloons]))

; Pair -> Pair
; on every clock tick subtract one from (pair-bakk
;; tests
(check-random
 (toc (make-pair 4 '()))
 (make-pair
  3 (cons (fetch-balloon (image-width LECTURE-HALL)
                         (image-height LECTURE-HALL)) '())))

(check-random
 (toc (make-pair 3 (cons (make-posn 3 4) '())))
 (make-pair
  2 (cons (fetch-balloon
           (image-width LECTURE-HALL)
           (image-height LECTURE-HALL)) (cons (make-posn 3 4) '()))))

(check-random
 (toc (make-pair 0 (cons (fetch-balloon COLS ROWS) '())))
 (make-pair 0 (cons (fetch-balloon COLS ROWS) '())))

(define (toc p)
  (cond
    [(= (pair-balloon# p) 0) p]
    [else
     (make-pair
      (sub1 (pair-balloon# p))
      (add-balloon
       (fetch-balloon
        (image-width LECTURE-HALL)
        (image-height LECTURE-HALL))
        (pair-lob p)))]))

; Posn List-of-posns -> List-of-posns
; Adds posn o to lop

(check-expect
 (add-balloon (make-posn 2 3) '())
 (cons (make-posn 2 3) '()))

(check-expect
 (add-balloon (make-posn 2 3) (cons (make-posn 3 4) '()))
 (cons (make-posn 2 3) (cons (make-posn 3 4) '())))

(define (add-balloon p lop) (cons p lop))

; X Y -> Posn
; for cols n and rows m returns a random posn with n and m
; boundaries

(check-random
 (fetch-balloon 4 8)
 (make-posn (random 4) (random 8)))

(define (fetch-balloon x y)
  (make-posn (random x) (random y)))

; Pair -> Image
; produces and image of lecture hall with red dots
; added as specified by Posns

;(check-expect (throw-balloons (make-pair 0 '())) SCENE)

(check-expect
 (throw-balloons (make-pair 2 (cons (make-posn 10 20) '())))
 (place-image RED-DOT 10 20 SCENE))

(check-expect
 (throw-balloons
  (make-pair 1 (cons (make-posn 10 20) (cons (make-posn 21 50) '()))))
 (place-image RED-DOT 21 50
              (place-image RED-DOT 10 20 SCENE)))

(define (throw-balloons p)
  (draw-balloons (pair-lob p)))


; ListOfPosns -> Image
; produces and image of lecture hall with red dots
; added as specified by Posns

(check-expect (draw-balloons '()) SCENE)

(check-expect
 (draw-balloons (cons (make-posn 10 20) '()))
 (place-image RED-DOT 10 20 SCENE))

(check-expect
 (draw-balloons
  (cons (make-posn 10 20) (cons (make-posn 21 50) '())))
 (place-image RED-DOT 21 50
              (place-image RED-DOT 10 20 SCENE)))

 (define (draw-balloons lop)
   (cond
     [(empty? lop) SCENE]
     [else
      (place-image
       RED-DOT
       (posn-x (first lop)) (posn-y (first lop))
       (draw-balloons (rest lop)))]))

;; Finding: It is not straightforward to code the big-bang's 'to-draw' without
;; using an auxiliary function; the reason is function throw-balloons receive
;; Pair and Pair is not a recursive function hence, self-referential algo will
;; not work.