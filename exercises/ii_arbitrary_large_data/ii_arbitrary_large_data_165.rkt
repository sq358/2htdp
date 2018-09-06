;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_165) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 165.

; ToyDescription is a one word string

; List-of-ToyDescriptions is one of
; - '()
; - (cons ToyDescriptions List-of-ToyDescriptions)

; List-of-ToyDescriptions -> List-of-ToyDescriptions
; replaces all occurances of "robot" in list of toys to
; "r2d2", all other Toys are the same

(check-expect (subst-robot '()) '())

(check-expect
 (subst-robot (cons "elo" (cons "robot" '())))
 (cons "elo" (cons "r2d2" '())))

(define (subst-robot lotd)
  (cond
    [(empty? lotd) '()]
    [else
     (cons
      (if (string=? (first lotd) "robot") "r2d2" (first lotd))
      (subst-robot (rest lotd)))]))

; String String List-of-Strings -> List-of-Strings
; replaces all occurances of string old with string new
; in the given list of strings

(check-expect (substitute "old" "new" '()) '())

(check-expect
 (substitute
  "elo" "yellow" (cons "elo" (cons "robot" '())))
 (cons "yellow" (cons "robot" '())))

(define (substitute old new los)
  (cond
    [(empty? los) '()]
    [else
     (cons
      (if (string=? (first los) old) new (first los))
      (substitute old new (rest los)))]))