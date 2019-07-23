;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_195) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 195.

(require 2htdp/batch-io)

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; On OS X: 
(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; Letter Dictionary -> Number
; counts the number of words in the given dictionary that
; start with the given letter

(check-expect
 (starts-with# "z" (list "apple" "animal" "book"))
 0)

(check-expect
 (starts-with# "a" (list "apple" "animal" "book"))
 2)

(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [else
     (if (string=? l (first (explode (first d))))
         (+ 1 (starts-with# l (rest d)))
         (starts-with# l (rest d)))]))

;; Observations
(starts-with# "e" AS-LIST)
(starts-with# "z" AS-LIST)