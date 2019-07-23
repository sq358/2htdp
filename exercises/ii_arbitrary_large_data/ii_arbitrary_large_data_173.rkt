;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_173) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 173.

(require 2htdp/batch-io)

(define TTT-PATH
  "/Users/maymandi/files/dev/bitbucket/racketlang/arash/2htdp/exercise_artifacts/ttt.txt")

(define ARTICLES
  (cons "a" (cons "an" (cons "the" '()))))

(define TEST-CONTENT
  (cons
   (cons "an" (cons "idealist" (cons "suffers" (cons "from" '()))))
   (cons
    (cons "the" (cons "lackies" (cons "of" (cons "a" (cons "system." '())))))
   '())))

; Los is one of
; - '()
; - (cons String Los)

; An LLS is one of: 
; – '()
; – (cons Los LLS)
; interpretation a list of lines, each is a list of Strings

; A Filename is a String

; Filename -> Filename
; removes articles from input filename in and writes the result
; to the output filename out

(check-error (rm-articles/file ""))

(check-expect
 (rm-articles/file "exercise-173-test.txt") "no-articles-exercise-173-test.txt")

(define (rm-articles/file f)
  (cond
    [(string=? f "") (error "input filename cannot be empty")]
    [else
     (write-file
      (string-append "no-articles-" f)
      (rm-articles/text (read-words/line f)))]))

; LLS -> String
; produces a string with all articles removed from the input string

(check-expect (rm-articles/text '()) "")

(check-expect
 (rm-articles/text TEST-CONTENT)
 "idealist suffers from\nlackies of system.")

(define (rm-articles/text lls)
  (cond
    [(empty? lls) ""]
    [(empty? (rest lls)) (line->string (first lls))]
    [else
     (string-append
      (line->string (first lls))
      "\n"
      (rm-articles/text (rest lls)))]))

; ListOfStrings -> String
; converts a line as list of strings to a string

(check-expect (line->string '()) "")

(check-expect
 (line->string (cons "hello" (cons "world" '())))
 "hello world")

(check-expect
 (line->string
  (cons "a"
        (cons "person,"
              (cons "an"
                    (cons "elephant,"
                          (cons "and"
                                (cons "the"
                                      (cons "bird" '()))))))))
 "person, elephant, and bird")

(define (line->string ln)
  (cond
    [(empty? ln) ""]
    [(member (first ln) ARTICLES)
     (line->string (rest ln))]
    [else
     (cond
       [(empty? (rest ln))
        (first ln)]
       [else
        (string-append (first ln) " " (line->string (rest ln)))])]))