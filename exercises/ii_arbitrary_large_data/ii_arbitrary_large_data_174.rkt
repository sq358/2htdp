;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_174) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 174.

(require 2htdp/batch-io)

; A Filename is a String

; ListOf1Strings is one of:
; - '()
; - (cons 1String ListOf1Strings

; Filename -> String
; encodes contents of input text file numerical string
; each letter in a word is encoded as numeric three-letter
; string with a value between 0 and 256

(check-error (encode-file ""))

(check-expect
 (encode-file "exercise-174-test.txt")
 (encode-lines
  (cons
   (cons "hello" '())
   (cons
    (cons "world" '()) '()))))

(define (encode-file f)
  (cond
    [(string=? f "") (error "input filename cannot be empty")]
    [else
     (encode-lines (read-words/line f))]))

; LLS -> String
; for each line ln in LLS encodes each letter of lines as
; three-letter string with a values between 0 and 256

(check-expect (encode-lines '()) "")

(check-expect
 (encode-lines
  (cons
   (cons "hello" '())
   (cons
    (cons "world" '())
    '())))
 (string-append
  "104101108108111" "119111114108100"))

(define (encode-lines lls)
  (cond
    [(empty? lls) ""]
    [else
     (string-append
      (encode-line (first lls))
      (encode-lines (rest lls)))]))

; ListOfStrings -> String
; for each string s in list of strings encodes s as
; three-letter string with values between 0 and 256
; and produces an output string from concatination of
; all the encodings

(check-expect (encode-line '()) "")

(check-expect
 (encode-line
  (cons "hello" (cons "word" '())))
  (string-append
   (encode-letters (explode "hello"))
   (encode-letters (explode "word"))))

(define (encode-line ln)
  (cond
    [(empty? ln) ""]
    [else
     (string-append
      (encode-letters (explode (first ln)))
      (encode-line (rest ln)))]))

; ListOf1Strings -> String
; for each 1string 1s in list of 1strings, encodes s as
; three-letter string with a values between 0 and 256

(check-expect (encode-letters '()) "")

(check-expect
 (encode-letters (explode "hello"))
 (string-append
  (encode-letter "h")
  (encode-letter "e")
  (encode-letter "l")
  (encode-letter "l")
  (encode-letter "o")))

(define (encode-letters los)
  (cond
    [(empty? los) ""]
    [else
     (string-append
      (encode-letter (first los))
      (encode-letters (rest los)))]))

; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))