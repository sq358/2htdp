;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_160) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 160.

(require 2htdp/universe)

; List-of-string String -> N
; determines how often s occurs in los

;; tests
(check-expect (count '() "jungle") 0)
(check-expect (count (cons "home" '()) "tree") 0)
(check-expect
 (count (cons "apple" (cons "orange" '())) "apple") 1)


(define (count los s)
  (cond
    [(empty? los) 0]
    [(string=? (first los) s) (+ 1 (count (rest los) s))]
    [else (count (rest los) s)]))

; Number Son.L -> Son.L
; removes x from s 
(define s1.L
  (cons 1 (cons 1 '())))
 
(check-expect
  (set-.L 1 s1.L) es)
 
(define (set-.L x s)
  (remove-all x s))