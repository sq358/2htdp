;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_198) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 198.

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

; A ListOfDictionaries is one:
; - '()
; - (cons Dictionary ListOfDictionaries)

(define-struct lc [letter count])
; A LetterCount is a structure
; (make-lc Letter Number)
; interpretation:
;  (make-lc l n) is the number of words n in a
;  dictionary that start with letter l

; ListOfLetterCounts is one of:
; - '()
; - (cons LetterCount ListOfLetterCounts)

;; ---------------------------------------------
;; Final Test
;; ---------------------------------------------
(check-expect
  (most-frequent.v1 AS-LIST)
  (most-frequent.v2 AS-LIST))

;; ---------------------------------------------
;; mostfrequent.v1
;; ---------------------------------------------
         
; Dictionary -> LetterCount
; produces the letter count for the letter that occurs most often
; as the first one in the give dictionary

(check-expect (most-frequent.v1 '()) '())

(check-expect
 (most-frequent.v1 (list "apple" "animal" "book" "car"))
 (most-frequent-lc
  (count-letters LETTERS (list "apple" "animal" "book" "car"))))

(define (most-frequent.v1 d)
  (cond
    [(empty? d) '()]
    [else
     (most-frequent-lc (count-letters LETTERS d))]))

; ListOfLetterCounts -> LetterCount
; picks the lettercount with the highest lc-count

(check-expect (most-frequent-lc '()) (make-lc "" 0))

(check-expect
 (most-frequent-lc
  (list (make-lc "a" 2) (make-lc "b" 1) (make-lc "c" 0)))
 (make-lc "a" 2))

(check-expect
 (most-frequent-lc
  (list (make-lc "a" 1) (make-lc "b" 2) (make-lc "c" 0)))
 (make-lc "b" 2))

(define (most-frequent-lc lcs)
  (cond
    [(empty? lcs) (make-lc "" 0)]
    [else
     (if
       (>= (lc-count (first lcs)) (lc-count (most-frequent-lc (rest lcs))))
       (first lcs)
       (most-frequent-lc (rest lcs)))]))

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

;; ---------------------------------------------
;; mostfrequent.v2
;; ---------------------------------------------

; Dictionary -> LetterCount
; produces the letter count for the letter that occurs most often
; as the first one in the give dictionary

(check-expect (most-frequent.v2 '()) '())

(check-expect
 (most-frequent.v2 (list "apple" "animal" "book" "car"))
 (compare
  (words-by-first-letter (list "apple" "animal" "book" "car"))))

(define (most-frequent.v2 d)
  (cond
    [(empty? d) '()]
    [else
     (compare (words-by-first-letter d))]))

; ListOfDictionaries -> LetterCount
; finds the longest dictionary and produces a letter count from it

(check-expect (compare '()) (make-lc "na" 0))
(check-expect
 (compare
  (list (list "apple" "animal") (list "book") (list "car")))
 (make-lc "a" 2))

(define (compare lod)
  (cond
    [(empty? lod) (make-lc "na" 0)]
    [else
     (compare-lettercounts
      (make-lc
       (first (explode (first (first lod))))
       (length (first lod)))
       (compare (rest lod)))]))

; LetterCount LetterCount -> LetterCount
; returns the lettercounts with the higher lc-count

(check-expect
 (compare-lettercounts (make-lc "na" 0) (make-lc "na" 0))
 (make-lc "na" 0))

(check-expect
 (compare-lettercounts (make-lc "s" 10) (make-lc "a" 20))
 (make-lc "a" 20))

(check-expect
 (compare-lettercounts (make-lc "h" 40) (make-lc "a" 20))
 (make-lc "h" 40))

(define (compare-lettercounts lc1 lc2)
  (if (>= (lc-count lc1) (lc-count lc2)) lc1 lc2))

; Dictionary -> ListOfDictionaries
; for the given dictionary, produces a list of dictionary one
; dictionary for each letter

(check-expect (words-by-first-letter '()) '())

(check-expect
 (words-by-first-letter (list "apple" "animal" "book" "car"))
 (remove-empty
  (words-in-dictionary
   LETTERS (list "apple" "animal" "book" "car"))))

(define (words-by-first-letter d)
  (remove-empty
   (words-in-dictionary LETTERS d)))

; ListOfDictionaries -> ListOfDictionaries
; removes (list '()) from the given list of dictionaries

(check-expect (remove-empty '()) '())
(check-expect (remove-empty (list '())) '())
(check-expect
 (remove-empty
  (list
   (list "apple" "animal") (list "book") (list "car") '()))
 (list (list "apple" "animal") (list "book") (list "car")))

(define (remove-empty lod)
  (cond
    [(empty? lod) '()]
    [else
      (if (empty? (first lod))
          (remove-empty (rest lod))
          (cons (first lod) (remove-empty (rest lod))))]))

; for the given dictionary, produces a list of dictionary one
; dictionary for item in the list of letters

(check-expect
 (words-in-dictionary
  (list "a" "b" "c" "d") (list "apple" "animal" "book" "car"))
 (list (list "apple" "animal") (list "book") (list "car") '()))

(define (words-in-dictionary letters d)
  (cond
    [(empty? letters) '()]
    [else
     (cons
      (create-dictionary (first letters) d)
      (words-in-dictionary (rest letters) d))]))

; Letter Dictionary -> Dictionary
; produces a dictionary containing all the words that start with
; the given letter in the given dictionary

(check-expect (create-dictionary "a" '()) '())
(check-expect
 (create-dictionary "a" (list "apple" "animal" "book" "car"))
 (list "apple" "animal"))

(check-expect
 (create-dictionary "d" (list "apple" "animal" "book" "car"))
 '())

(define (create-dictionary l d)
  (cond
    [(empty? d) '()]
    [else
     (if (string=? l (first (explode (first d))))
         (cons (first d) (create-dictionary l (rest d)))
         (create-dictionary l (rest d)))]))