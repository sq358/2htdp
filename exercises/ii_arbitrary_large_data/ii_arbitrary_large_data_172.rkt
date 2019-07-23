;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_172) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 172.

(require 2htdp/batch-io)

(define TTT-PATH
  "/Users/maymandi/files/dev/bitbucket/racketlang/arash/2htdp/exercise_artifacts/ttt.txt")

; Los is one of
; - '()
; - (cons String Los)

; An LLS is one of: 
; – '()
; – (cons Los LLS)
; interpretation a list of lines, each is a list of Strings
 
(define line0 (cons "hello" (cons "world" '())))
(define line1
  (cons "Space"
        (cons "is"
              (cons "the"
                    (cons "final"
                          (cons "frontier" '()))))))

(define line2 '())
(define lls0 '())
(define lls1 (cons line0 (cons line1 (cons line2 '()))))

; LLS -> String
; converts a list of lines to a string

(check-expect (collapse lls0) "")
(check-expect
 (collapse lls1)
 "hello world\nSpace is the final frontier\n")

(define (collapse lls)
  (cond
    [(empty? lls) ""]
    [else
     (string-append
      (line->string (first lls))
      (if (empty? (rest lls)) "" "\n")
      (collapse (rest lls)))]))

; ListOfStrings -> String
; converts a line as list of strings to a string

(check-expect (line->string '()) "")
(check-expect (line->string line0) "hello world")

(define (line->string ln)
  (cond
    [(empty? ln) ""]
    [else
     (cond
       [(empty? (rest ln)) (first ln)]
       [else
        (string-append
         (first ln) " " (line->string (rest ln)))])]))