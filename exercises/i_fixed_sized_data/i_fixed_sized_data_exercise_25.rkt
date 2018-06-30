; Exercise 25.
(require 2htdp/image)

(define (image-classify img)
  (cond
    [(>= (image-height img) (image-width img)) "tall"]
    [(= (image-height img) (image-width img)) "square"]
    [(<= (image-height img) (image-width img)) "wide"]))

; For a 'tall' image it did not suggest a fix.
; <some-image> is a placeholder for an actual image
