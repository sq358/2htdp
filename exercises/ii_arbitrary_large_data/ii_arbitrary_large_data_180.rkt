;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_180) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 180.

; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 

; Lo1s -> Image
; creates an image from the input list of 1strings

(check-expect
 (editor-text
  (cons "h" (cons "e" (cons "l" (cons "l" (cons "o" '()))))))
 (text "hello" FONT-SIZE FONT-COLOR))

(define (editor-text s)
  (text (lo1s->string s) FONT-SIZE FONT-COLOR))

; Lo1s -> String
; produces a string from the input list of 1strings

(check-expect (lo1s->string '()) "")
(check-expect
 (lo1s->string (cons "h" (cons "e" (cons "y" '()))))
 "hey")

(define (lo1s->string lo1s)
  (cond
    [(empty? lo1s) ""]
    [else
     (string-append (first lo1s) (lo1s->string (rest lo1s)))]))
  