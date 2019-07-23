;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname min-lon) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; ListOfNumbers -> Number
; retruns input list of numbers minimum value

(check-expect (lon.min '()) '())
(check-expect (lon.min (list 1 5 0)) 0)

(define (lon.min lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (if (empty? (rest lon))
         (first lon)
         (min (first lon) (lon.min (rest lon))))]))