;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_169) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 169.

(define LOWER-X 0)
(define LOWER-Y 0)

(define UPPER-X 100)
(define UPPER-Y 200)

; ListOfPosns -> ListOfPosns
; produces a list-of-posns from all the posns in input
; whose x-coordinates are between 0 and 100 and whose
; y-coordinates are between 0 to 200

(check-expect (legal '()) '())

(check-expect
 (legal (cons (make-posn 2 4) (cons (make-posn 101 3) '())))
 (cons (make-posn 2 4) '()))

(check-expect
 (legal
  (cons (make-posn 2 4) (cons (make-posn 50 199) '())))
 (cons (make-posn 2 4) (cons (make-posn 50 199) '())))

(check-expect
 (legal
  (cons (make-posn 0 0) (cons (make-posn 50 199) '())))
 (cons (make-posn 0 0) (cons (make-posn 50 199) '())))

(check-expect
 (legal
  (cons (make-posn 100 200) (cons (make-posn 50 199) '())))
 (cons (make-posn 100 200) (cons (make-posn 50 199) '())))

(check-expect
 (legal
  (cons (make-posn 101 201) (cons (make-posn 4 45) '())))
 (cons (make-posn 4 45) '()))

(define (legal lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop)
     (cond
       [(within-range? (first lop))
        (cons (first lop) (legal (rest lop)))]
        [else (legal (rest lop))])]))

; Posn -> Boolean
; for (make-posn x y) p determines if:
;  LOWER-X <= (posn-x p) <= UPPER-X and ... 
;  LOWER-Y <= (posn-y p) <= UPPER-Y and ... 

(check-expect
 (within-range? (make-posn 0 0)) #true)

(check-expect
 (within-range? (make-posn 50 150)) #true)

(check-expect
 (within-range? (make-posn 100 200)) #true)

(check-expect
 (within-range? (make-posn 101 201)) #false)

(define (within-range? p)
  (and
   (and (>= (posn-x p) LOWER-X) (<= (posn-x p) UPPER-X))
   (and (>= (posn-y p) LOWER-Y) (<= (posn-y p) UPPER-Y))))