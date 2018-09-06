;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_162) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 162.

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours

(check-expect (wage* '()) '())
(check-expect
 (wage* (cons 28 '())) (cons 336 '()))
(check-expect
 (wage* (cons 4 (cons 2 '())))
 (cons 48 (cons 24 '())))
(check-error (wage* 101))

(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [(> (first whrs) 100)
     (error "Error: working hours exceeds 100 hrs. per week")]
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))

; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* 12 h))