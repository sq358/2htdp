;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_142) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 142.

(define TEST-SQ-IM1
  (rectangle 10 10 "solid" "red"))

(define TEST-SQ-IM2
  (rectangle 10 10 "solid" "blue"))

(define TEST-NON-SQ-IM1
  (rectangle 5 10 "solid" "blue"))

; ImageOrFalse is one of:
; - Image
; - #false

; ListOfImages is either
; - '()
; - ListOfImages

; ListOfImages PositiveNumber -> ImageOrFalse
; for positive number n produces the first image on the
; list-of-images that is not an n by n square if no
; images found it produces #false

;;tests
(check-expect (ill-sized? '() 2) #false)
(check-expect (ill-sized? (cons TEST-SQ-IM1 '()) 10) #false)
(check-expect (ill-sized? (cons TEST-NON-SQ-IM1 '()) 10)
              TEST-NON-SQ-IM1)
(check-expect
 (ill-sized? (cons TEST-SQ-IM1 (cons TEST-SQ-IM2 '())) 10) #false)

(check-expect
 (ill-sized? (cons TEST-SQ-IM1 (cons TEST-SQ-IM2 '())) 5)
  TEST-SQ-IM1)

(define (ill-sized? loi n)
  (cond
    [(<= n 0)
      (error "image size n can't be negative or zero")]
    [(empty? loi) #false]
    [(not (is-n-sq? (first loi) n)) (first loi)]
    [else (ill-sized? (rest loi) n)]))


; Image PositiveNumber
; for positive number n returns #true if image image im is
; n by n; otherwise, returns #false

(check-expect
 (is-n-sq? TEST-SQ-IM1 10) #true)

(check-expect
 (is-n-sq? TEST-SQ-IM1 5) #false)

(check-error
 (is-n-sq? TEST-SQ-IM1 0)
 (error "image size n can't be negative or zero"))

(check-error
 (is-n-sq? TEST-SQ-IM1 2)
 (error "image size n can't be negative or zero"))
     
(define (is-n-sq? im n)
  (cond
    [(<= n 0)
     (error "image size n can't be negative or zero")]
    [else
     (and
      (= (image-width im) (image-height im))
      (= (image-width im) n))]))