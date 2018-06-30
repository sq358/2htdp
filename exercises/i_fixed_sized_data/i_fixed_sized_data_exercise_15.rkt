; Exercise 15.
(define (==> sunny friday)
  (if (or
       (boolean=? sunny #false)
       (boolean=? friday #true))
      #true #false))
