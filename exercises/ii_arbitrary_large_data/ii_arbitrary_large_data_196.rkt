;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_196) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 196.

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

(define-struct lc [letter count])
; A LetterCount is a structure
; (make-lc Letter Number)
; interpretation:
;  (make-lc l n) is the number of words n in a
;  dictionary that start with letter l

; ListOfLetterCounts is one of:
; - '()
; - (cons LetterCount ListOfLetterCounts)

; Dictionary -> ListOfLetterCounts
; for each item in dictionary, counts how often each letter is
; used as the first one of a word in the given dictionary

(check-expect (count-by-letter '()) '())

(check-expect
 (count-by-letter (list "apple" "animal" "book" "car"))
 (count-letters LETTERS (list "apple" "animal" "book" "car")))

(define (count-by-letter d)
  (cond
    [(empty? d) '()]
    [else
     (count-letters LETTERS d)]))

; ListOfLetters Dictionary -> ListOfLetterCounts
; for each item in list of letters, counts how often each letter is
; used as the first one of a word in the given dictionary

(check-expect
 (count-letters '() (list "apple" "animal" "book" "car"))
 '())

(check-expect
 (count-letters (list "a" "b") '())
 (list (make-lc "a" 0) (make-lc "b" 0))) 

(check-expect
 (count-letters (list "a" "b") (list "apple" "animal" "book" "car"))
 (list (make-lc "a" 2) (make-lc "b" 1)))

(check-expect
 (count-letters
  (list "a" "b" "c" "d") (list "apple" "animal" "book" "car"))
 (list
  (make-lc "a" 2) (make-lc "b" 1) (make-lc "c" 1) (make-lc "d" 0)))

(define (count-letters letters d)
  (cond
    [(empty? letters) '()]
    [else
     (cons
      (make-lc (first letters) (starts-with# (first letters) d))
      (count-letters (rest letters) d))]))
     
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

(define (count-words letters)
  (cond
    [(empty? letters) 0]
    [else
     (+ (lc-count (first letters)) (count-words (rest letters)))]))
