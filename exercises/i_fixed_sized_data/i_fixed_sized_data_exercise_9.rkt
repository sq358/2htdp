; Excercise 9.
(define (convert-to-positive x)
  (if (string? x) (string-length x)
      (if (integer? x)
          (if (or (= x 0) (< x 0)) x (- x 1))
      (if (image? x) (* (image-width x) (image-height x))
          (if (boolean? x)
              (if (and x #true) 10 20) x)))))
