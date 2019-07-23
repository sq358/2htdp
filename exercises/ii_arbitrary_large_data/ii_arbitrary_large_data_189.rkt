;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_189) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 189.

; Number List-of-numbers -> Boolean
; determines whether some number n is in a descending sorted list of numbers

(check-expect (search 10 '()) #false)
(check-expect (search 10 (list 20 6 5)) #false)
(check-expect (search 10 (list 40 30 10 6)) #true)

(define (search n lon)
  (cond
    [(empty? lon) #false]
    [(cons? lon)
     (cond
       [(= n (first lon)) #true]
       [(> n (first lon)) #false]
       [else
        (search n (rest lon))])]))