;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_246__249) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 246.

(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l)
                   (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))

(extract < (cons 6 (cons 4 '())) 5)

Exercise 247.

(extract < (cons 8 (cons 4 '())) 5)

; Exercise 248.

; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))

(squared>? 3 10)

(squared>? 4 10)

(extract squared>? (list 3 4 5) 10)

; Exercise 249.

(define (f x) x)

(cons f '())

(f f)

(cons f (cons 10 (cons (f 10) '())))
