; Exercise 24.
(define (==> x y)
  (or (not x) y))

(==> #true #false)
