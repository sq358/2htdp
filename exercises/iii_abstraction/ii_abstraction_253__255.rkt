;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_253__255) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 253.

; [Number -> Boolean]
(zero? 3)

; [Boolean String -> Boolean]
(empty? "hello")

; [Number Number Number -> Number]

; [Number -> [List-of Number]]
; [[List-of Number] -> Boolean]

; Exercise 254.

; sort-n
; [List-of Number] [Number Number -> Boolean] -> [List-of Number]

; sort-s
; [List-of String] [String String -> Boolean] -> [List-of String]

; [X Y] [List-of X] [X X -> Y] -> [List-of X]

; [List-of IR] [IR IR -> Boolean] -> [List-of IR]

; 255.

; map-n
; [List-of Number] [Number -> Number] -> [List-of Number]

; map-s
; [List-of String] [Sting -> String] -> [List-of String]

; [X] [List-of X] [X -> X] -> [List-of X]

