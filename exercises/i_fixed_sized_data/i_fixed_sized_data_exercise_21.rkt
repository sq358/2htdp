; Exercise 21.
(define (ff a)
  (* 10 a))

(ff (ff 1))

; DrRacket’s stepper does not reuse the results of computations
(+ (ff 1) (ff 1))

