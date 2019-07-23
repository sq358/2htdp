;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_197) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 197.

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

; Dictionary -> LetterCount
; produces the letter count for the letter that occurs most often
; as the first one in the give dictionary

(check-expect (most-frequent.v2 '()) '())

(check-expect
 (most-frequent.v2 (list "apple" "animal" "book" "car"))
 (first
  (sort-by-count
   (count-letters LETTERS (list "apple" "animal" "book" "car")))))

(define (most-frequent.v2 d)
  (cond
    [(empty? d) '()]
    [else
     (first
      (sort-by-count
       (count-letters LETTERS d)))]))

; ListOfLetterCounts -> ListOfLetterCounts
; produces sorted list of counts by lc-count in descending order

(check-expect (sort-by-count '()) '())

(check-expect
 (sort-by-count
  (list
   (make-lc "a" 2)
   (make-lc "b" 3)
   (make-lc "c" 1)))
 (list
  (make-lc "b" 3)
  (make-lc "a" 2)
  (make-lc "c" 1)))

(define (sort-by-count lcs)
  (cond
    [(empty? lcs) '()]
    [else
     (insert-lc (first lcs) (sort-by-count (rest lcs)))]))

; LetterCount ListOfLetterCounts -> ListOfLetterCounts
; inserts letter count lc into given list of letter counts

(check-expect
 (insert-lc (make-lc "c" 1) '()) (list (make-lc "c" 1)))

(check-expect
 (insert-lc
  (make-lc "b" 3) (list (make-lc "c" 1)))
 (list (make-lc "b" 3) (make-lc "c" 1)))

(check-expect
 (insert-lc
  (make-lc "a" 2) (list (make-lc "b" 3) (make-lc "c" 1)))
 (list (make-lc "b" 3) (make-lc "a" 2) (make-lc "c" 1)))

(define (insert-lc l lcs)
  (cond
    [(empty? lcs) (list l)]
    [else
     (if (>= (lc-count l) (lc-count (first lcs)))
         (cons l lcs)
         (cons (first lcs) (insert-lc l (rest lcs))))]))

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


; (most-frequent.v1 AS-LIST)
; (make-lc "s" 22759)

;; Q. Consider designing both. Which one do you prefer? Why?
;; A. I prefer most-frequent.v2 which uses sorted list of pairs
;;    because it's faster.