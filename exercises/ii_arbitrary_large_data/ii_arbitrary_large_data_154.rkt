;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_154) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 154.

(define-struct layer [color doll])

; An RD (short for Russhian doll) is one of:
; - String
; - (make-layer String RD)

; RD -> Number
; how many dolls are part of an-rd

(check-expect (depth "red") 1)
(check-expect
  (depth
   (make-layer "yellow" (make-layer "green" "red")))
  3)

(define (depth an-rd)
  (cond
    [(string? an-rd) 1]
    [else
     (+ (depth (layer-doll an-rd)) 1)]))

; RD -> String
; produces a comma-separated string of RD's doll colors
; from outside to inside

(check-expect (colors "red") "red")
(check-expect
 (colors
  (make-layer "red" (make-layer "green" "blue")))
 "red, green, blue")

(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [else
     (string-append
      (layer-color an-rd) ", " (colors (layer-doll an-rd)))]))