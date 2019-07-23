;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_256__257) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 256.

(define (f x)
  (* -1 x))

(argmax f '(-1 -2 -3))

; argmin
; find the (first) item in lx that minimizes f
; if (argmin f (list x-1, ..., x-n)) = x-i,
; then (<= (f x-i) (f x-1)), (<= (f x-i) (f x-2), ...

; Exercise 257.

; [x] X [X -> [List-of X]] -> [List-of X]
; build-l*st works just like build-list
; constructs a list by applying f to numbres
; between 0 to n-1

(check-expect (build-l*st 0 add1) '())
(check-expect (build-l*st 3 add1) '(1 2 3))

(define (build-l*st n f)
  (add-to-end 0 n f))

(define (add-to-end m n f)
  (cond
    [(= m n) '()]
    [else
     (cons (f m) (add-to-end (add1 m) n f))]))