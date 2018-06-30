; Exercise 19.
(define (string-insert s i)
  (cond
   [(and (>= i 0) (< i (string-length s)))
         (string-append
         (substring s 0 i) "_" (substring s i))]
   [else s]))
