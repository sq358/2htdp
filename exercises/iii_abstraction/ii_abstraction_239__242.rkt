;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ii_abstraction_239__242) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 239.

; A [List X Y] is a structure: 
;   (cons X (cons Y '()))

; A [List NUMBER NUMBER] is a structure:
;   (cons Number (cons Number '())

(cons 4 (cons 3 '()))

; A [List NUMBER 1STRING] is a structure:
;   (cons Number (cons 1String '())

(cons 3 (cons "a" '()))

; A [List STRING BOOLEAN] is a structure:
;   (cons String (cons Boolean '())

(cons "hello" (cons #true '()))

; Exercise 240.

(define-struct layer [stuff])

; An LStr is one of: 
; – String
; – (make-layer LStr)

; An LNum is one of: 
; – Number
; – (make-layer LNum)

; A [LITM T] is one of:
; - T
; - (make-layer LITM)

; A [LITM String] is one of:
; - String
; - (make-layer [LITM String])

; A [LITM Number] is one of:
; - Number
; - (make-layer [LITM Number])

; Exercise 241.

; A NEList-of-temperatures is one:
; - (cons Temperature '())
; - (cons Temperature NEList-of-temperatures)

; A NEList-of-Booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-Booleans

; A [NELIST ITEM] is one of:
; - (cons ITEM '())
; - (cons ITEM [NELIST ITEM])

; Exercise 242.

; A [Maybe X] is one of: 
; – #false 
; – X

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise 
(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(define (occurs s los)
  (cond
    [(empty? (rest los)) #f]
    [else
     (if (string=? s (first los))
         (rest los)
         (occurs s (rest los)))]))
