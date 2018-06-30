; Exercise 20.
(define (tall-or-wide i)
 (if (> (image-height i) (image-width i)) "tall"
     (if (= (image-height i) (image-width i)) "square" "wide")
))
