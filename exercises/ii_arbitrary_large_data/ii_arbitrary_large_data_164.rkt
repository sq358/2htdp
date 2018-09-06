;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_164) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 164.

; N is one of
; - 0
; - (add1 N)

; List-of-Numbers is one of
; - '()
; - (cons N List-of-USD)

(define USD-EUR 0.86)

; List-of-Numbers -> List-of-Numbers
; converts list of usd to list of eoro

(check-expect (convert-euro '()) '())
(check-error (convert-euro (cons 0 (cons 2 '()))))
(check-error (convert-euro (cons 5 (cons -1 '()))))

(check-expect
 (convert-euro (cons 1 (cons 54 '())))
 (cons (* USD-EUR 1) (cons (* 54 USD-EUR) '())))


(define (convert-euro lousd)
  (cond
    [(empty? lousd) '()]
    [(<= (first lousd) 0)
     (error "Error: USD value cannot be less or equal than 0")]
    [else
     (cons (* USD-EUR (first lousd)) (convert-euro (rest lousd)))]))


; List-of-Numbers N -> List-of-Numbers
; converts list of usd to list of euror based on the input
; exchange rate

(check-expect (convert-euro* '() .93) '())
(check-error (convert-euro* (cons 0 (cons 2 '())) .93))
(check-error (convert-euro* (cons 5 (cons -1 '())) .56))
(check-expect
 (convert-euro* (cons 1 (cons 54 '())) .99)
 (cons (* 1 .99) (cons (* 54 .99) '())))

(define (convert-euro* lousd rate)
  (cond
    [(empty? lousd) '()]
    [(<= (first lousd) 0)
     (error "Error: USD value cannot be less or equal than 0")]
    [else
     (cons (* rate (first lousd)) (convert-euro* (rest lousd) rate))]))