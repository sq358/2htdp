; Exercise 58.

(define LOW-PRICE 1000)
(define LUXURY-PRICE 10000)

; A Price falls into one of three intervals:
; — 0 through less than 1000
; — 1000 through less than 10000
; — 10000 and above.
; interpretation the price of an item

;; tests
(check-expect (sales-tax 0) 0)
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax LOW-PRICE) (* 0.05 LOW-PRICE))
(check-expect (sales-tax LUXURY-PRICE) (* 0.08 LUXURY-PRICE))
(check-expect (sales-tax 12017) (* 0.08 12017))

(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p LOW-PRICE)) 0]
    [(and (<= 1000 p) (< p LUXURY-PRICE)) (* 0.05 p)]
    [(>= p 10000) (* 0.08 p)]))
