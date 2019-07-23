;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_175) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 175.

(require 2htdp/batch-io)

(define ERROR-MSG
  "filename cannot be empty")

(define TEST-LOS
  (cons "hello" (cons "world" '())))

(define TEST-LLS
  (cons (cons "hello" '()) (cons (cons "world" '()) '())))

; A Filename is a non-empty String

(define-struct result [1s w l]) 
; WcResult is a structure:
;  (make-result Number Number Number)
; interpretation (make-result 10 2 2) is word count result
; with 10 1Strings 2 words and 2 lines

; Filename -> WcResult
; computes the word count result for the file provided as
; input file name

(check-error (wc ""))
(check-expect
 (wc "exercise-175-test.txt")
 (make-result 10 2 2))

(define (wc f)
  (cond
    [(string=? f "") (error ERROR-MSG)]
    [else
     (make-result
      (wc/1string f) (wc/word f) (wc/line f))]))

; Filename -> Number
; computes the number of 1strings in the file provided as
; input file name

(check-error (wc/1string ""))
(check-expect
 (wc/1string "exercise-175-test.txt") 10)

(define (wc/1string f)
  (cond
    [(string=? f "") (error ERROR-MSG)]
    [else (count/1string (read-words f))]))

; LoS -> Number
; computes the number of 1strings in the input list of strings

(check-expect (count/1string '()) 0)
(check-expect (count/1string TEST-LOS) 10)

(define (count/1string los)
  (cond
    [(empty? los) 0]
    [else
     (+
      (length (explode (first los))) (count/1string (rest los)))]))

; Filename -> Number
; computes the number of words in the file provided as
; input file name

(check-error (wc/word ""))
(check-expect
 (wc/word "exercise-175-test.txt") 2)

(define (wc/word f)
  (cond
    [(string=? f "") (error ERROR-MSG)]
    [else (length (read-words f))])) 

; Filename -> Number
; computes the number of lines in the file provided as
; input file name

(check-error (wc/line ""))
(check-expect
 (wc/line "exercise-175-test.txt") 2)

(define (wc/line f)
  (cond
    [(string=? f "") (error ERROR-MSG)]
    [else (length (read-words/line f))])) 
