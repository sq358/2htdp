; Exercise 12.
(define (calc-distance x y)
  (sqrt (+ (sqr x) (sqr y))))

(define (calc-eqcube-volume x)
  (expt x 3))

(define (calc-eqcube-surface x)
  (* 6 (expt x 2)))
