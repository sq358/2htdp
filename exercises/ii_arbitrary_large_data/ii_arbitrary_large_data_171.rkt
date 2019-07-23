;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_171) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 171.

(require 2htdp/batch-io)

(define TTT-PATH
  "/Users/maymandi/files/dev/bitbucket/racketlang/arash/2htdp/exercise_artifacts/ttt.txt")

; ListOfStrings is one of
; - '()
; - (cons String ListOfString)

; Exercise. Represent Piet Hein's poem as an instance of
; ListOfStrings where each line is represented as a string

(define POEM.v1 (cons "TTT"
 (cons ""
  (cons "Put up in a place"
   (cons "where it's easy to see"
    (cons "the cryptic admonishment"
     (cons "T.T.T."
      (cons ""
       (cons "When you feel how depressingly"
        (cons "slowly you climb,"
         (cons "it's well to remember that"
          (cons "Things Take Time."
           (cons ""
            (cons "Piet Hein" '()))))))))))))))

(read-lines TTT-PATH)

; Exercise. Represent Piet Hein's poem as an instance of
; ListOfStrings where each word is represented as a string

(define POEM.v2
  (cons "TTT"
   (cons "Put"
    (cons "in"
     (cons "up"
      (cons "in"
       (cons "a"
        (cons "place"
         (cons "where"
          (cons "it's"
           (cons "easy"
            (cons "to"
             (cons "see"
              (cons "the"
               (cons "cryptic"
                (cons "admonishment"
                 (cons "T.T.T" '()))))))))))))))))) ; ignored the rest

(read-words TTT-PATH)

; ListOfListOfStrings is one of:
; - '()
; - (cons ListOfStrings ListOfListOfStrings)

(cons
 (cons "TTT" '())
 (cons
  '()
  (cons
   (cons "Put" (cons "up" (cons "in" (cons "a" (cons "place" '())))))
   (cons
    (cons "where" (cons "it's" (cons "easy" (cons "to" (cons "see" '())))))
    (cons
     (cons "the" (cons "cryptic" (cons "admonishment" '())))
     (cons
      (cons "T.T.T." '())
      (cons
       '()
       (cons
        (cons
         "When"
         (cons "you" (cons "feel" (cons "how" (cons "depressingly" '())))))
        (cons
         (cons "slowly" (cons "you" (cons "climb," '())))
         (cons
          (cons "it's" (cons "well" (cons "to" (cons "remember" (cons "that" '())))))
          (cons
           (cons "Things" (cons "Take" (cons "Time." '())))
           (cons '() (cons (cons "Piet" (cons "Hein" '())) '())))))))))))))

(read-words/line TTT-PATH)
 