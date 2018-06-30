; Exercise 17.
(define (image-classify i)
  (if (> (image-height i) (image-width i)) "tall"
      (if (> (image-height i) (image-width i)) "wide" "square")))
